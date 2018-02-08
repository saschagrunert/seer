-- | This module everything related to the main library interface
--
-- @since 0.1.0
module Seer
  ( MonadSeer
  , getStorages
  , version
  ) where

import Data.Bifunctor         (bimap)
import Data.List              (transpose)
import Seer.Storage           (list)
import Text.PrettyPrint.Boxes (hsep, left, render, text, vcat)

class Monad m => MonadSeer m where
  -- A 'list' wrapper
  list' :: m (Either IOError [[String]])

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadSeer IO where
  list' = list

-- | The version of the library
--
-- @since 0.1.0
version :: String
version = "0.1.0"

-- | Creates a string table from a two dimensional String list
--
-- @since 0.1.0
tablyfy :: [[String]] -> String
tablyfy r = render . hsep 2 left $ vcat left . map text <$> transpose r

-- | The standard message when nothing was found
--
-- @since 0.1.0
nf :: String
nf = "Nothing found\n"

-- | The standard table header for a name
--
-- @since 0.1.0
name :: String
name = "NAME"

-- | The standard table header for a remote
--
-- @since 0.1.0
remote :: String
remote = "REMOTE"

-- | List all available Storages if possible. Exaluates to either a fully
-- formatted table (Right) or an error message (Left).
--
-- @since 0.1.0
getStorages :: (MonadSeer m) => m (Either String String)
getStorages =
  bimap
    show
    (\r ->
       if null r
         then nf
         else tablyfy ([name, remote] : r)) <$>
  list'
