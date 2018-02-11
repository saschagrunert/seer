-- | This module includes everything related to Configuration management.
--
-- @since 0.1.0
{-# LANGUAGE DeriveGeneric #-}

module Seer.Config
  ( Config
  , ConfigSpec(..)
  , new
  , toList
  ) where

import Data.Yaml     (FromJSON, ToJSON)
import GHC.Generics  (Generic)
import Seer.Manifest (ApiVersion (V1), Manifest (Manifest),
                      ResourceKind (Config), ToList (toList),
                      newMetadata)

-- | A synonym for the Config
--
-- @since 0.1.0
type Config = Manifest ConfigSpec

-- | The Config specification
--
-- @since 0.1.0
newtype ConfigSpec = ConfigSpec
  { storage :: String
  } deriving (Eq, Generic, Show)

-- | Parses the 'ConfigSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ConfigSpec

-- | Generates the YAML/JSON from an 'ConfigSpec'
--
-- @since 0.1.0
instance ToJSON ConfigSpec

-- | Encode a 'ConfigSpec' as a list of Strings
--
-- @since 0.1.0
instance ToList ConfigSpec where
  toList x = pure $ storage x

-- | Create a new default configuration
--
-- @since 0.1.0
new
  :: String    -- ^ The name of the storage
  -> IO Config -- ^ The result
new n = (\m -> Manifest V1 Config m $ ConfigSpec n) <$> newMetadata
