-- | This module includes everything about a 'Schedule'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Seer.Schedule
  ( MonadSchedule
  , Schedule
  , ScheduleSpec
  , new
  , newSpec
  , start
  , resourceID
  , actionID
  ) where

import Control.Lens    (makeLenses)
import Data.Time.Clock (UTCTime)
import Data.UUID       (UUID)
import Data.Yaml       (FromJSON, ToJSON)
import GHC.Generics    (Generic)
import Seer.Manifest   (ApiVersion (V1)
                       ,Manifest
                       ,Metadata
                       ,ResourceKind (Schedule)
                       ,currentMetadata
                       ,newManifest)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadSchedule m where
  currentMetadata' :: m Metadata

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadSchedule IO where
  currentMetadata' = currentMetadata

-- | A synonym for a Schedule
--
-- @since 0.1.0
type Schedule = Manifest ScheduleSpec

-- | The data specified for a Schedule
--
-- @since 0.1.0
data ScheduleSpec = ScheduleSpec
  { _start      :: UTCTime -- ^ The start schedule
  , _resourceID :: UUID    -- ^ The referenced 'Resource'
  , _actionID   :: UUID    -- ^ The referenced 'Action'
  } deriving (Eq, Generic, Ord, Show)

makeLenses ''ScheduleSpec

-- | Parses the 'ScheduleSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ScheduleSpec

-- | Generates the YAML/JSON from an 'ScheduleSpec'
--
-- @since 0.1.0
instance ToJSON ScheduleSpec

-- | Get a new 'ScheduleSpec' for a given start, Resource and Action ID
--
-- @since 0.1.0
newSpec
  :: UTCTime      -- ^ The name of the Resource
  -> UUID         -- ^ The description of the Resource
  -> UUID         -- ^ The Availabilities of the Resource
  -> ScheduleSpec -- ^ The result
newSpec = ScheduleSpec

-- | Generates a new 'Manifest' including a 'ScheduleSpec'
--
-- @since 0.1.0
new
  :: MonadSchedule m
  => UTCTime     -- ^ The start date
  -> UUID        -- ^ The referenced 'Resource'
  -> UUID        -- ^ The referenced 'Action'
  -> m Schedule  -- ^ The result
new a b c =
  (\m -> newManifest V1 Schedule m $ newSpec a b c) <$> currentMetadata'
