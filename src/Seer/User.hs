-- | This module includes everything about a 'User'.

module Seer.User (
    newEmptyUser,
    User(..),
) where

import Seer.Action (Action)

-- | The data specified for an User
data User = User {
    name     :: String,     -- ^ The name of the User
    actions  :: [Action]    -- ^ The 'Action's of the User
} deriving (Eq, Show)

-- | Creates a new User from a given name without containing any 'Action'
newEmptyUser
    :: String     -- ^ The users name
    -> User       -- ^ The resulting 'User'
newEmptyUser a = User {name = a, actions = []}
