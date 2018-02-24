-- | This module includes everything about a 'Action'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Action
  ( Action
  , ActionSpec(..)
  , MonadAction
  , new
  ) where

import           Data.Maybe    (fromMaybe)
import           Data.Yaml     (FromJSON
                               ,ToJSON)
import           GHC.Generics  (Generic)
import           Seer.Manifest (ApiVersion (V1)
                               ,Manifest (Manifest)
                               ,Metadata
                               ,ResourceKind (Action)
                               ,ToList (headers
                                       ,toList)
                               ,newMetadata)
import           Seer.Time     (Duration
                               ,parseDuration)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadAction m where
  newMetadata' :: m Metadata

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadAction IO where
  newMetadata' = newMetadata

-- | A synonym for an Action
--
-- @since 0.1.0
type Action = Manifest ActionSpec

-- | The data specified for a Action
--
-- @since 0.1.0
data ActionSpec = ActionSpec
  { name        :: String       -- ^ The name of the Action
  , description :: Maybe String -- ^ The general description of the Action
  , duration    :: Duration     -- ^ The duration of the Action
  } deriving (Eq, Generic, Show)

-- | Parses the 'ActionSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ActionSpec

-- | Generates the YAML/JSON from an 'ActionSpec'
--
-- @since 0.1.0
instance ToJSON ActionSpec

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
  (\m -> (Manifest V1 Action m . ActionSpec a b) <$> parseDuration c)
    <$> newMetadata'

-- | Encode an 'ActionSpec' as a list of Strings
--
-- @since 0.1.0
instance ToList ActionSpec where
  headers _ = ["NAME", "DESCRIPTION", "DURATION"]
  toList x = [a, b, c]
    where
      a = name x
      b = fromMaybe "" $ description x
      c = show $ duration x
