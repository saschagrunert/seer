-- | This module includes everything about a 'User'.

{-# LANGUAGE DeriveGeneric #-}

module Seer.User (
    (|-),
    (|+),
    User,
    Users,
    empty,
    name,
    newUser,
    toUsers,
) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Seer (Id)
import qualified Data.Set as S (Set, delete, empty, insert, singleton)

-- | The data representing multiple Users
--
-- @since 0.1.0
newtype Users = Users (S.Set User)
    deriving (Eq, Generic, Show)

-- | The data representing a single User
--
-- @since 0.1.0
newtype User = User { name :: Id -- ^ The name of the user
                    } deriving (Eq, Generic, Ord, Show)

-- | Parses the 'User' from YAML/JSON
instance FromJSON User

-- | Parses the 'Users' from YAML/JSON
instance FromJSON Users

-- | Generates the YAML/JSON from an 'User'
instance ToJSON User

-- | Generates the YAML/JSON from 'Users'
instance ToJSON Users

-- | The empty 'Users' representation
--
-- Examples:
--
-- >>> empty
-- Users (fromList [])
--
-- @since 0.1.0
empty :: Users
empty = Users S.empty

-- | Creates a new 'User' from a given name
--
-- Examples:
--
-- >>> newUser "Username"
-- User {name = "Username"}
--
-- @since 0.1.0
newUser
    :: Id   -- ^ The users name
    -> User -- ^ The resulting 'User'
newUser a = User {name = a}

-- | Converts a 'User' to 'Users'
--
-- Examples:
--
-- >>> toUsers $ newUser "User"
-- Users (fromList [User {name = "User"}])
--
-- @since 0.1.0
toUsers
    :: User  -- ^ The User to be added
    -> Users -- ^ The Users where the user should be added
toUsers x = Users $ S.singleton x

-- | Adds a 'User' to 'Users'
--
-- Examples:
--
-- >>> empty |+ (newUser "User")
-- Users (fromList [User {name = "User"}])
--
-- @since 0.1.0
infixr 5 |+
(|+)
    :: Users -- ^ The Users where the user should be added
    -> User  -- ^ The User to be added
    -> Users -- ^ The resulting Users
Users xs |+ x = Users $ S.insert x xs

-- | Removes a 'User' from 'Users'
--
-- Examples:
--
-- >>> let user = newUser "User"
-- >>> let users = toUsers user
-- >>> users |- user
-- Users (fromList [])
--
-- @since 0.1.0
infixr 5 |-
(|-)
    :: Users -- ^ The Users where the user should be removed
    -> User  -- ^ The User to be removed
    -> Users -- ^ The resulting Users
Users xs |- x = Users $ S.delete x xs
