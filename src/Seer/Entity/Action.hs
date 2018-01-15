-- | This module includes everything about a 'Action'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Entity.Action (
    Action(..),
) where

import Data.Time.Clock (NominalDiffTime)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | The data specified for a Action
--
-- @since 0.1.0
data Action = Action { name :: String              -- ^ The name of the Action
                     , description :: Maybe String -- ^ The general description of the Action
                     , duration :: NominalDiffTime -- ^ The duration of the Action
                     } deriving (Eq, Generic, Show)

-- | Parses the 'Action' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Action

-- | Generates the YAML/JSON from an 'Action'
--
-- @since 0.1.0
instance ToJSON Action
