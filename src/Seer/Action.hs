-- | This module includes everything about a 'Action'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Seer.Action
  ( Action
  , ActionSpec
  , MonadAction
  , description
  , duration
  , name
  , new
  , newSpec
  ) where

import Control.Lens  (makeLenses
                     ,(^.))
import Data.Maybe    (fromMaybe)
import Data.Yaml     (FromJSON
                     ,ToJSON)
import GHC.Generics  (Generic)
import Seer.Manifest (ApiVersion (V1)
                     ,Manifest
                     ,Metadata
                     ,ResourceKind (Action)
                     ,ToList (headers
                             ,toList)
                     ,currentMetadata
                     ,newManifest)
import Seer.Time     (Duration
                     ,parseDuration)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadAction m where
  currentMetadata' :: m Metadata

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadAction IO where
  currentMetadata' = currentMetadata

-- | A synonym for an Action
--
-- @since 0.1.0
type Action = Manifest ActionSpec

-- | The data specified for a Action
--
-- @since 0.1.0
data ActionSpec = ActionSpec
  { _name        :: String       -- ^ The name of the Action
  , _description :: Maybe String -- ^ The general description of the Action
  , _duration    :: Duration     -- ^ The duration of the Action
  } deriving (Eq, Generic, Ord, Show)

makeLenses ''ActionSpec

-- | Parses the 'ActionSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ActionSpec

-- | Generates the YAML/JSON from an 'ActionSpec'
--
-- @since 0.1.0
instance ToJSON ActionSpec

-- | Get a new 'ActionSpec' for a given name, description and duration
--
-- @since 0.1.0
newSpec
  :: String           -- ^ The name of the Resource
  -> Maybe String     -- ^ The description of the Resource
  -> Duration         -- ^ The Duration in minutes
  -> ActionSpec       -- ^ The result
newSpec = ActionSpec

-- | Generates a new 'Manifest' including a 'ActionSpec'
--
-- @since 0.1.0
new
  :: MonadAction m
  => String            -- ^ The name of the Action
  -> Maybe String      -- ^ The description of the Action
  -> String            -- ^ The duration of the Action
  -> m (Maybe Action)  -- ^ The result
new a b c =
  (\m -> (newManifest V1 Action m . newSpec a b) <$> parseDuration c)
    <$> currentMetadata'

-- | Encode an 'ActionSpec' as a list of Strings
--
-- @since 0.1.0
instance ToList ActionSpec where
  headers _ = ["NAME", "DESCRIPTION", "DURATION"]
  toList x = [a, b, c]
    where
      a = _name x
      b = fromMaybe "" $ x ^. description
      c = show $ x ^. duration
