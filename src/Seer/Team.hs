-- | This module includes everything about a 'Team'.

module Seer.Team (
    Team,
    Teams,
    empty,
    name,
    newTeam,
    users,
) where

import qualified Seer.User as User
import qualified Data.Map as Map

-- | The data representing multiple Teams
newtype Teams = Teams (Map.Map String Team)
    deriving (Show)

-- | The data representing a single Team
data Team = Team { name     :: String       -- ^ The name of the Team
                 , users    :: User.Users   -- ^ The 'Users' of a Team
                 } deriving (Eq, Show)

-- | The empty 'Teams' representation
empty :: Teams
empty = Teams Map.empty

-- | Creates a new 'Team' from a given name with empty 'Users'
newTeam
    :: String   -- ^ The teams name
    -> Team     -- ^ The resulting 'Team'
newTeam a = Team {name = a, users = User.empty}
