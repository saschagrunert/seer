-- | This module includes everything about a 'Action'.

module Seer.Action (
    Action,
    Actions,
    assigned,
    empty,
    name,
    newAction,
) where

import Seer.Team (Team)

-- | The data representing multiple Actions
newtype Actions = Actions [Action]
    deriving (Show)

-- | The data specified for a Action
data Action = Action { name :: String           -- ^ The identifier of the Action
                     , assigned :: Maybe Team   -- ^ To whom belongs the Action
                     } deriving (Eq, Show)

-- | The empty 'Actions' representation
empty :: Actions
empty = Actions []

-- | Creates a new 'Action' from a given name
newAction
    :: String     -- ^ The actions name
    -> Action     -- ^ The resulting 'Action'
newAction a = Action {name = a, assigned = Nothing}
