-- | This module includes everything about a 'Action'.

{-# LANGUAGE DeriveGeneric #-}

module Seer.Action (
    Action,
    Actions,
    assigned,
    empty,
    name,
    newAction,
) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Seer (Id)

-- | The data representing multiple Actions
--
-- @since 0.1.0
newtype Actions = Actions [Action]
    deriving (Eq, Generic, Show)

-- | The data specified for a Action
--
-- @since 0.1.0
data Action = Action { name :: String         -- ^ The identifier of the Action
                     , assigned :: Maybe [Id] -- ^ To whom belongs the Action
                     } deriving (Eq, Generic, Show)

-- | Parses the 'Action' from YAML/JSON
instance FromJSON Action

-- | Parses the 'Actions' from YAML/JSON
instance FromJSON Actions

-- | Generates the YAML/JSON from an 'Action'
instance ToJSON Action

-- | Generates the YAML/JSON from 'Actions'
instance ToJSON Actions

-- | The empty 'Actions' representation
--
-- Examples:
--
-- >>> empty
-- Actions []
--
-- @since 0.1.0
empty :: Actions
empty = Actions []

-- | Creates a new 'Action' from a given name
--
-- Examples:
--
-- >>> newAction "doSomething"
-- Action {name = "doSomething", assigned = Nothing}
--
-- @since 0.1.0
newAction
    :: Id     -- ^ The actions name
    -> Action -- ^ The resulting 'Action'
newAction a = Action {name = a, assigned = Nothing}
