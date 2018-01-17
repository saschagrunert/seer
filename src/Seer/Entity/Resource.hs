-- | This module includes everything about a 'Resource'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Entity.Resource (
    Resource(..),
) where

import Seer.Availability (Availabilities)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | The data specified for a Resource
--
-- @since 0.1.0
data Resource = Resource { name :: String                   -- ^ The name of the Resource
                         , description :: Maybe String      -- ^ The general description of the Resource
                         , availabilities :: Availabilities -- ^ The availabilities of the Resource
                         } deriving (Eq, Generic, Show)

-- | Parses the 'Resource' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Resource

-- | Generates the YAML/JSON from an 'Resource'
--
-- @since 0.1.0
instance ToJSON Resource
