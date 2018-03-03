-- | This module includes utility functions
--
-- @since 0.1.0

module Seer.Utils
  ( rstrip
  , (>>-)
  ) where

import Data.Char (isSpace)

-- | Helper for monadic 'Either' standard handling
--
-- @since 0.1.0
(>>-)
  :: Monad m
  => m (Either a b)        -- ^ The monadic value to be unwrapped
  -> (b -> m (Either a c)) -- ^ The function to be applied if Either is 'Right'
  -> m (Either a c)        -- ^ The result
a >>- f = a >>= either (return . Left) f

-- | Drops all trailing whitespace from a String
--
-- @since 0.1.0
rstrip
  :: String -- ^ The String to be stripped
  -> String -- ^ The result
rstrip = reverse . dropWhile isSpace . reverse
