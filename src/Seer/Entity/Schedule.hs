-- | This module includes everything about a 'Schedule'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Entity.Schedule (
    Schedule(..),
) where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | The data specified for a Schedule
--
-- @since 0.1.0
data Schedule = Schedule { from :: UTCTime      -- ^ The start schedule
                         , to :: UTCTime        -- ^ The end schedule
                         , resourceID :: UUID   -- ^ The referenced 'Resrouce'
                         , actionID :: UUID     -- ^ The referenced 'Action'
                         } deriving (Eq, Generic, Show)

-- | Parses the 'Schedule' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Schedule

-- | Generates the YAML/JSON from an 'Schedule'
--
-- @since 0.1.0
instance ToJSON Schedule
