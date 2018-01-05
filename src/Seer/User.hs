-- | This module includes everything about a 'User'.

module Seer.User (
    User,
    Users,
    empty,
    newUser,
) where

import qualified Data.Set as Set

-- | The data representing multiple Users
newtype Users = Users (Set.Set User)
    deriving (Eq, Show)

-- | The data representing a single User
newtype User = User String
    deriving (Eq, Show)

-- | The empty 'Users' representation
empty :: Users
empty = Users Set.empty

-- | Creates a new 'User' from a given name
newUser
    :: String   -- ^ The users name
    -> User     -- ^ The resulting 'User'
newUser = User
