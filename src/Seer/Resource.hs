-- | This module includes everything about a 'Resource'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Seer.Resource (
    Resource,
    Resources,
    empty,
    name,
    newResource,
    toResources,
) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Seer (Id)
import Seer.Entity (EntityRelation(..))
import qualified Data.Set as S (Set, delete, empty, insert, singleton)

-- | The data representing multiple Resources
--
-- @since 0.1.0
newtype Resources = Resources (S.Set Resource)
    deriving (Eq, Generic, Ord, Show)

-- | The data representing a single Resource
--
-- @since 0.1.0
newtype Resource = Resource { name :: Id -- ^ The name of the resource
                            } deriving (Eq, Generic, Ord, Show)

-- | Parses the 'Resource' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Resource

-- | Parses the 'Resources' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Resources

-- | Generates the YAML/JSON from an 'Resource'
--
-- @since 0.1.0
instance ToJSON Resource

-- | Generates the YAML/JSON from 'Resources'
--
-- @since 0.1.0
instance ToJSON Resources

-- | Entity relations between 'Resource' and 'Resources'
--
-- @since 0.1.0
instance EntityRelation Resource Resources where
    -- | Removes a 'Resource' from 'Resources'
    --
    -- @since 0.1.0
    Resources xs |- x = Resources $ S.delete x xs

    -- | Adds a 'Resource' to 'Resources'
    --
    -- @since 0.1.0
    Resources xs |+ x = Resources $ S.insert x xs

-- | The empty 'Resources' representation
--
-- Examples:
--
-- >>> empty
-- Resources (fromList [])
--
-- @since 0.1.0
empty :: Resources
empty = Resources S.empty

-- | Creates a new 'Resource' from a given name
--
-- Examples:
--
-- >>> newResource "Resourcename"
-- Resource {name = "Resourcename"}
--
-- @since 0.1.0
newResource
    :: Id   -- ^ The resources name
    -> Resource -- ^ The resulting 'Resource'
newResource a = Resource {name = a}

-- | Converts a 'Resource' to 'Resources'
--
-- Examples:
--
-- >>> toResources $ newResource "Resource"
-- Resources (fromList [Resource {name = "Resource"}])
--
-- @since 0.1.0
toResources
    :: Resource  -- ^ The Resource to be added
    -> Resources -- ^ The Resources where the resource should be added
toResources x = Resources $ S.singleton x
