-- | This module includes everything about a 'User'.

{-# LANGUAGE DeriveGeneric #-}

module Seer.User (
    User,
    Users,
    empty,
    name,
    newUser,
) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Seer (Id)
import qualified Data.Set as S (empty, Set)

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
