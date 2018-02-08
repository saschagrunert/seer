-- | This module includes everything related to Configuration management.
--
-- @since 0.1.0
{-# LANGUAGE DeriveGeneric #-}

module Seer.Config
  ( Config
  , ConfigSpec(..)
  , new
  ) where

import Data.Yaml     (FromJSON, ToJSON)
import GHC.Generics  (Generic)
import Seer.Manifest (ApiVersion (V1), Manifest (Manifest),
                      ResourceKind (Config), newMetadata)

-- | A synonym for the Config
--
-- @since 0.1.0
type Config = Manifest ConfigSpec

-- | The Config specification
--
-- @since 0.1.0
newtype ConfigSpec = ConfigSpec
  { storage :: Maybe String
  } deriving (Eq, Generic, Show)

-- | Parses the 'ConfigSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ConfigSpec

-- | Generates the YAML/JSON from an 'ConfigSpec'
--
-- @since 0.1.0
instance ToJSON ConfigSpec

-- | Create a new default configuration
--
-- @since 0.1.0
new :: IO Config -- ^ The result
new = (\m -> Manifest V1 Config m $ ConfigSpec Nothing) <$> newMetadata
