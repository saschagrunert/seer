-- | This module includes everything about a 'Schedule'.
--
-- @since 0.1.0
{-# LANGUAGE DeriveGeneric #-}

module Seer.Schedule
  ( Schedule
  , ScheduleSpec(..)
  , new
  , defaultTimeFormat
  ) where

import Data.Time.Clock  (UTCTime)
import Data.Time.Format (FormatTime, defaultTimeLocale, formatTime)
import Data.UUID        (UUID)
import Data.Yaml        (FromJSON, ToJSON)
import GHC.Generics     (Generic)
import Seer.Manifest    (ApiVersion (V1), Manifest (Manifest),
                         ResourceKind (Schedule), ToList (toList),
                         newMetadata)

-- | A synonym for a Schedule
--
-- @since 0.1.0
type Schedule = Manifest ScheduleSpec

-- | The data specified for a Schedule
--
-- @since 0.1.0
data ScheduleSpec = ScheduleSpec
  { from       :: UTCTime -- ^ The start schedule
  , to         :: UTCTime -- ^ The end schedule
  , resourceID :: UUID -- ^ The referenced 'Resrouce'
  , actionID   :: UUID -- ^ The referenced 'Action'
  } deriving (Eq, Generic, Show)

-- | Parses the 'ScheduleSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ScheduleSpec

-- | Generates the YAML/JSON from an 'ScheduleSpec'
--
-- @since 0.1.0
instance ToJSON ScheduleSpec

-- | Encode a 'ScheduleSpec' as a list of Strings
--
-- @since 0.1.0
instance ToList ScheduleSpec where
  toList x = [a, b, c, d]
    where
      a = defaultTimeFormat $ from x
      b = defaultTimeFormat $ to x
      c = "" -- TODO: link to Resource name
      d = "" -- TODO: link to Action name

-- | The default time formatting output
--
-- @since 0.1.0
defaultTimeFormat :: FormatTime a => a -> String
defaultTimeFormat = formatTime defaultTimeLocale "%D"

-- | Generates a new 'Manifest' including a 'ScheduleSpec'
--
-- @since 0.1.0
new
  :: UTCTime     -- ^ The start date
  -> UTCTime     -- ^ The end date
  -> UUID        -- ^ The referenced 'Resource'
  -> UUID        -- ^ The referenced 'Action'
  -> IO Schedule -- ^ The result
new a b c d =
  (\m -> Manifest V1 Schedule m $ ScheduleSpec a b c d) <$> newMetadata
