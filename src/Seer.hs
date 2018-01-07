-- | This module everything related to the main library interface

module Seer (
    Id,
    someFunc,
) where

-- | A simple identifier used by certain data types
--
-- @since 0.1.0
type Id = String

someFunc :: IO ()
someFunc = putStrLn "someFunc"
