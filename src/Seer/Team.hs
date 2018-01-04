-- | This module includes everything about a 'Team'.

module Seer.Team (
    newEmptyTeam,
    Team(..),
) where

import Seer.Action (Action)
import Seer.User (User)

-- | The data specified for a Team
data Team = Team {
    name     :: String,     -- ^ The name of the Team
    actions  :: [Action],   -- ^ The 'Action's of the Team
    users    :: [User]      -- ^ The 'User's of a Team
} deriving (Eq, Show)

-- | Creates a new Team from a given name without containing any 'User' or
-- 'Action'
newEmptyTeam
    :: String     -- ^ The teams name
    -> Team       -- ^ The resulting 'Team'
newEmptyTeam a = Team {name = a, actions = [], users = []}
