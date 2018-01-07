-- | This module includes everything about a 'Team'.

{-# LANGUAGE DeriveGeneric #-}

module Seer.Team (
    Team,
    Teams,
    empty,
    name,
    newTeam,
    users,
) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Seer (Id)
import qualified Data.Set as S (empty, Set)

-- | The data representing multiple Teams
--
-- @since 0.1.0
newtype Teams = Teams (S.Set Team)
    deriving (Eq, Generic, Show)

-- | The data representing a single Team
--
-- @since 0.1.0
data Team = Team { name     :: Id         -- ^ The name of the Team
                 , users    :: Maybe [Id] -- ^ The related 'Users' of a Team
                 } deriving (Eq, Generic, Ord, Show)

-- | Parses the 'Team' from YAML/JSON
instance FromJSON Team

-- | Parses the 'Teams' from YAML/JSON
instance FromJSON Teams

-- | Generates the YAML/JSON from an 'Team'
instance ToJSON Team

-- | Generates the YAML/JSON from 'Teams'
instance ToJSON Teams

-- | The empty 'Teams' representation
--
-- Examples:
--
-- >>> empty
-- Teams (fromList [])
--
-- @since 0.1.0
empty :: Teams
empty = Teams S.empty

-- | Creates a new 'Team' from a given name with empty 'Users'
--
-- Examples:
--
-- >>> newTeam "myTeam"
-- Team {name = "myTeam", users = Nothing}
--
-- @since 0.1.0
newTeam
    :: Id   -- ^ The teams name
    -> Team -- ^ The resulting 'Team'
newTeam a = Team {name = a, users = Nothing}
