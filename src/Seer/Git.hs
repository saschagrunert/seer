-- | This module includes everything related to git.

{-# LANGUAGE OverloadedStrings #-}

module Seer.Git (
    MonadGit,
    runGitCommand,
) where

import Control.Exception (SomeException, try)
import System.Exit (ExitCode(..))
import System.Process

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
    try' :: m a -> m (Either SomeException a)

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadGit IO where
    readProcessWithExitCode' = readProcessWithExitCode
    try' = try

-- | Run a git command, capture its standard output/error as 'String' and
-- wait for it to complete. Returns 'Either' an error (Left) or the succeeding
-- message (Right)
--
-- Examples:
--
-- >>> :m +Data.Either
-- >>> runGitCommand "add ."
-- Right ""
--
-- >>> runGitCommand "failure"
-- Left "git: 'failure' is not a git command. See 'git --help'.\n"
--
-- @since 0.1.0
runGitCommand
    :: MonadGit m
    => String                   -- ^ The command to executed
    -> m (Either String String) -- ^ 'Either' the error or the output
runGitCommand a = do
    r <- try' $ readProcessWithExitCode' "git" (words a) ""
    case r of
        Left  err -> return . Left $ show err
        Right res -> evaluateProcessResult res

-- | Turns an 'ExitCode' and two 'String' for standard output/error and
-- turns them into either an error or an output 'String'
--
-- @since 0.1.0
evaluateProcessResult
    :: MonadGit m
    => (ExitCode, String, String) -- ^ The outputs of the process
    -> m (Either String String)   -- ^ The 'Either' result
evaluateProcessResult (e, out, err) = case e of
    ExitFailure _ -> return . Left $ err
    ExitSuccess   -> return . Right $ out
