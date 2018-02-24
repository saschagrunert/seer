-- | This module includes utility functions
--
-- @since 0.1.0

module Seer.Utils
  ( rstrip
  ) where

import Data.Char (isSpace)

-- | Drops all trailing whitespace from a String
--
-- @since 0.1.0
rstrip
  :: String -- ^ The String to be stripped
  -> String -- ^ The result
rstrip = reverse . dropWhile isSpace . reverse
