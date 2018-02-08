-- | This module includes everything related to git.
--
-- @since 0.1.0

{-# LANGUAGE OverloadedStrings #-}

module Seer.Git (MonadGit
                ,runGitCommand
                ,runGitCommandIO) where

import Control.Exception (try)
import Data.Either       (either)
import System.Directory  (withCurrentDirectory)
import System.Exit       (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Error   (IOError, userError)
import System.Process    (readProcessWithExitCode)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadGit m where
    -- A 'readProcessWithExitCode' wrapper
    readProcessWithExitCode'
        :: FilePath
        -> [String]
        -> String
        -> m (ExitCode, String, String)

    -- A 'try' wrapper
    try' :: m a -> m (Either IOError a)

    -- A 'withCurrentDirectory' wrapper
    withCurrentDirectory' :: FilePath -> m a -> m a

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadGit IO where
    readProcessWithExitCode' = readProcessWithExitCode
    try' = try
    withCurrentDirectory' = withCurrentDirectory

-- | Run a git command, capture its standard output/error as 'String' and
-- wait for it to complete. Returns 'Either' an error (Left) or the succeeding
-- message (Right)
--
-- Examples:
--
-- >>> :m +Data.Either
-- >>> runGitCommand "add ." "."
-- Right ""
--
-- >>> runGitCommand "failure" "."
-- Left "git: 'failure' is not a git command. See 'git --help'.\n"
--
-- @since 0.1.0
runGitCommand
  :: MonadGit m
  => String                   -- ^ The command to executed
  -> String                   -- ^ The working directory
  -> m (Either String String) -- ^ 'Either' the error or the output
runGitCommand a d = either (Left . show) f
  <$> try' (withCurrentDirectory' d $ readProcessWithExitCode' "git" (split a) "")
 where
  f (ExitFailure _, _, e) = Left e
  f (ExitSuccess  , o, _) = Right o
  split = outside [] . (' ' :)
  outside res xs = case xs of
    ' ':' ' :ys -> outside res $ ' ' : ys
    ' ':'\'':ys -> res ++ inside [] ys
    ' '     :ys -> res ++ outside [] ys
    c       :ys -> outside (add c res) ys
    _           -> res
  inside res xs = case xs of
    ' ' :' ':   ys -> inside res $ ' ' : ys
    '\'':' ':   ys -> res ++ outside [] (' ' : ys)
    [       '\'']  -> res
    c       :   ys -> inside (add c res) ys
    _              -> res
  add c res = if null res then [[c]] else map (++ [c]) res

-- | Run a "git" command, capture its standard error as 'IOError'. Returns the
-- empty tuple if the command succeeded.
--
-- Examples:
--
-- >>> :m +Data.Either
-- >>> runGitCommandIO "add ." "."
-- Right ()
--
-- >>> runGitCommandIO "failure" "."
-- Left user error (git: 'failure' is not a git command. See 'git --help'.
-- )
--
-- @since 0.1.0
runGitCommandIO
  :: MonadGit m
  => String                -- ^ The command
  -> String                -- ^ The working directory
  -> m (Either IOError ()) -- ^ The result
runGitCommandIO c d = runGitCommand c d >>= either (return . Left . userError) (\_ -> return $ Right ())
