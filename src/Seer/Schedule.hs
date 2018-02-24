-- | This module includes everything about a 'Schedule'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Schedule
  ( MonadSchedule
  , Schedule
  , ScheduleSpec(..)
  , new
  ) where

import Data.Time.Clock (UTCTime)
import Data.UUID       (UUID)
import Data.Yaml       (FromJSON, ToJSON)
import GHC.Generics    (Generic)
import Seer.Manifest   (ApiVersion (V1)
                       ,Manifest (Manifest)
                       ,Metadata
                       ,ResourceKind (Schedule)
                       ,newMetadata)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadSchedule m where
  newMetadata' :: m Metadata

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadSchedule IO where
  newMetadata' = newMetadata

-- | A synonym for a Schedule
--
-- @since 0.1.0
type Schedule = Manifest ScheduleSpec

-- | The data specified for a Schedule
--
-- @since 0.1.0
data ScheduleSpec = ScheduleSpec
  { start      :: UTCTime -- ^ The start schedule
  , resourceID :: UUID    -- ^ The referenced 'Resource'
  , actionID   :: UUID    -- ^ The referenced 'Action'
  } deriving (Eq, Generic, Show)

-- | Parses the 'ScheduleSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ScheduleSpec

-- | Generates the YAML/JSON from an 'ScheduleSpec'
--
-- @since 0.1.0
instance ToJSON ScheduleSpec

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
  (\m -> Manifest V1 Schedule m $ ScheduleSpec a b c) <$> newMetadata'
