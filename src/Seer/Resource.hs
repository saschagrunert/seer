-- | This module includes everything about a 'Resource'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Seer.Resource
  ( MonadResource
  , Resource
  , ResourceSpec
  , availabilities
  , description
  , name
  , new
  , newSpec
  ) where

import           Control.Lens   (makeLenses
                                ,(^.))
import           Data.Maybe     (fromMaybe)
import           Data.Yaml      (FromJSON
                                ,ToJSON)
import           GHC.Generics   (Generic)
import           Seer.Manifest  (ApiVersion (V1)
                                ,Manifest
                                ,Metadata
                                ,ResourceKind (Resource)
                                ,ToList(headers)
                                ,currentMetadata
                                ,newManifest)
import qualified Seer.Time as T (Availabilities
                                ,toList)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadResource m where
  currentMetadata' :: m Metadata

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadResource IO where
  currentMetadata' = currentMetadata

-- | A synonym for a Resource
--
-- @since 0.1.0
type Resource = Manifest ResourceSpec

-- | The data specified for a Resource
--
-- @since 0.1.0
data ResourceSpec = ResourceSpec
  { _name           :: String           -- ^ The name of the Resource
  , _description    :: Maybe String     -- ^ The general description of the Resource
  , _availabilities :: T.Availabilities -- ^ The availabilities of the Resource
  } deriving (Eq, Generic, Ord, Show)

makeLenses ''ResourceSpec

-- | Parses the 'ResourceSpec' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON ResourceSpec

-- | Generates the YAML/JSON from an 'ResourceSpec
--
-- @since 0.1.0
instance ToJSON ResourceSpec

-- | Encode a 'ResourceSpec' as a list of Strings
--
-- @since 0.1.0
instance ToList ResourceSpec where
  headers l = ["NAME", "DESCRIPTION"] ++ headers (_availabilities l)
  toList x = [a, b] ++ c
    where
      a = _name x
      b = fromMaybe "" $ x ^. description
      c = T.toList $ x ^. availabilities

-- | Get a new 'ResourceSpec' for a given name, description and availabilities
--
-- @since 0.1.0
newSpec
  :: String           -- ^ The name of the Resource
  -> Maybe String     -- ^ The description of the Resource
  -> T.Availabilities -- ^ The Availabilities of the Resource
  -> ResourceSpec     -- ^ The result
newSpec = ResourceSpec

-- | Generates a new 'Manifest' including a 'ResourceSpec'
--
-- @since 0.1.0
new
  :: MonadResource m
  => String           -- ^ The name of the Resource
  -> Maybe String     -- ^ The description of the Resource
  -> T.Availabilities -- ^ The Availabilities of the Resource
  -> m Resource       -- ^ The result
new a b c =
  (\m -> newManifest V1 Resource m $ newSpec a b c) <$> currentMetadata'
