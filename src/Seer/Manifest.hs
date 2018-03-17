-- | This module includes everything about a the global object 'Manifest'
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Seer.Manifest
  ( ApiVersion(V1)
  , Manifest
  , MonadManifest
  , Metadata
  , ResourceKind(Action
                ,Config
                ,Resource
                ,Schedule)
  , ToList(headers
          ,toList)
  , apiVersion
  , creationTimestamp
  , currentMetadata
  , kind
  , metadata
  , newManifest
  , newMetadata
  , spec
  , uid
  ) where

import Control.Lens    (makeLenses
                       ,(^.))
import Data.Time.Clock (UTCTime
                       ,getCurrentTime)
import Data.UUID       (UUID)
import Data.UUID.V4    (nextRandom)
import Data.Yaml       (FromJSON
                       ,ToJSON)
import GHC.Generics    (Generic)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadManifest m where
  getCurrentTime' :: m UTCTime
  nextRandom' :: m UUID

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadManifest IO where
  getCurrentTime' = getCurrentTime
  nextRandom' = nextRandom

-- | The data specified for a 'Manifest'
--
-- @since 0.1.0
data Manifest s = Manifest
  { _apiVersion :: ApiVersion   -- ^ The API Version
  , _kind       :: ResourceKind -- ^ The resource Kind
  , _metadata   :: Metadata     -- ^ Resource metadata
  , _spec       :: s            -- ^ The specification of the Resource
  } deriving (Eq, Generic, Ord, Show)

-- | Parses the 'Manifest' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON s => FromJSON (Manifest s)

-- | Generates the YAML/JSON from an 'Manifest'
--
-- @since 0.1.0
instance ToJSON s => ToJSON (Manifest s)

-- | Generic conversion to a list of Strings
--
-- @since 0.1.0
class ToList a where
  headers :: a -> [String]
  toList :: a -> [String]

-- | The available API versions
--
-- @since 0.1.0
data ApiVersion
  = V1 -- ^ Version 1
  | V2 -- ^ Version 2
  deriving (Eq, Enum, Generic, Ord, Show)

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
  deriving (Eq, Generic, Ord, Show)


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
  { _creationTimestamp :: UTCTime -- ^ The creation time of the Metadata
  , _uid               :: UUID    -- ^ The unique ID for this Metadata
  } deriving (Eq, Generic, Ord, Show)

makeLenses ''Metadata
makeLenses ''Manifest

-- | Parses the 'Metadata' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Metadata

-- | Generates the YAML/JSON from an 'Metadata'
--
-- @since 0.1.0
instance ToJSON Metadata

-- | For every Manifest the toList instance will be mapped to the spec
--
-- @since 0.1.0
instance ToList s => ToList (Manifest s) where
  headers l = headers $ l ^. spec
  toList l = toList $ l ^. spec

-- | Create a new Manifest for a given 'ApiVersion', 'ResourceKind', 'Metadata'
-- and Spec
--
-- @since 0.1.0
newManifest :: ApiVersion -> ResourceKind -> Metadata -> s -> Manifest s
newManifest = Manifest

-- | Create a new Metadata for a given 'UTCTime' and 'UUID'
--
-- @since 0.1.0
newMetadata :: UTCTime -> UUID -> Metadata
newMetadata = Metadata

-- | Generates a new 'Metadata' from the current UTC time and a random UUID
--
-- @since 0.1.0
currentMetadata :: MonadManifest m => m Metadata
currentMetadata = do
  time <- getCurrentTime'
  uuid <- nextRandom'
  return $ Metadata time uuid
