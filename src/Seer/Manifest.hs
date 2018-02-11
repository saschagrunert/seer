-- | This module includes everything about a the global object 'Manifest'
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Manifest
  ( ApiVersion(..)
  , IsManifest(..)
  , Manifest(..)
  , Metadata(..)
  , ResourceKind(..)
  , ToList(..)
  , newMetadata
  ) where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID       (UUID, toString)
import Data.UUID.V4    (nextRandom)
import Data.Yaml       (FromJSON, ToJSON)
import GHC.Generics    (Generic)

-- | The data specified for a 'Manifest'
--
-- @since 0.1.0
data Manifest s = Manifest
  { apiVersion :: ApiVersion   -- ^ The API Version
  , kind       :: ResourceKind -- ^ The resource Kind
  , metadata   :: Metadata     -- ^ Resource metadata
  , spec       :: s            -- ^ The specification of the Resource
  } deriving (Eq, Generic, Show)

-- | Parses the 'Manifest' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON s => FromJSON (Manifest s)

-- | Generates the YAML/JSON from an 'Manifest'
--
-- @since 0.1.0
instance ToJSON s => ToJSON (Manifest s)

-- | Generic UUID retrieval class for Manifests
--
-- @since 0.1.0
class IsManifest a where
  uuidString :: a -> String

-- | Generic conversion to a list of Strings
--
-- @since 0.1.0
class ToList a where
  toList :: a -> [String]

-- | For every Manifest the toList instance will be mapped to the spec
--
-- @since 0.1.0
instance ToList s => ToList (Manifest s) where
  toList = toList . spec

-- | The implementation of the UUID retrieval
--
-- @since 0.1.0
instance IsManifest (Manifest s) where
  uuidString = toString . uid . metadata

-- | The available API versions
--
-- @since 0.1.0
data ApiVersion
  = V1 -- ^ Version 1
  | V2 -- ^ Version 2
  deriving (Eq, Enum, Generic, Show)

-- | Parses the 'Version' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ApiVersion

-- | Generates the YAML/JSON from an 'Version'
--
-- @since 0.1.0
instance ToJSON ApiVersion

-- | The available API Kinds
--
-- @since 0.1.0
data ResourceKind
  = Config   -- ^ References a 'Config'
  | Action   -- ^ References a 'Action'
  | Resource -- ^ References a 'Resource'
  | Schedule -- ^ References a 'Schedule'
  deriving (Eq, Generic, Show)


-- | Parses the 'ResourceKind' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ResourceKind

-- | Generates the YAML/JSON from an 'ResourceKind'
--
-- @since 0.1.0
instance ToJSON ResourceKind

-- | The data specified for a Metadata object
--
-- @since 0.1.0
data Metadata = Metadata
  { creationTimestamp :: UTCTime -- ^ The creation time of the Metadata
  , uid               :: UUID    -- ^ The unique ID for this Metadata
  } deriving (Eq, Generic, Show)

-- | Parses the 'Metadata' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Metadata

-- | Generates the YAML/JSON from an 'Metadata'
--
-- @since 0.1.0
instance ToJSON Metadata

-- | Generates a new 'Metadata' from the current UTC time and a random UUID
--
-- @since 0.1.0
newMetadata :: IO Metadata
newMetadata = do
  time <- getCurrentTime
  uuid <- nextRandom
  return Metadata {creationTimestamp = time, uid = uuid}

