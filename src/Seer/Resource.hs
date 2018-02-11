-- | This module includes everything about a 'Resource'.
--
-- @since 0.1.0
{-# LANGUAGE DeriveGeneric #-}

module Seer.Resource
  ( Resource
  , ResourceSpec(..)
  , new
  ) where

import           Data.Maybe             (fromMaybe)
import           Data.Yaml              (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Seer.Availability      (Availabilities)
import qualified Seer.Availability as A (toList)
import           Seer.Manifest          (ApiVersion (V1), Manifest (Manifest),
                                         ResourceKind (Resource), ToList,
                                         newMetadata)

-- | A synonym for a Resource
--
-- @since 0.1.0
type Resource = Manifest ResourceSpec

-- | The data specified for a Resource
--
-- @since 0.1.0
data ResourceSpec = ResourceSpec
  { name           :: String         -- ^ The name of the Resource
  , description    :: Maybe String   -- ^ The general description of the Resource
  , availabilities :: Availabilities -- ^ The availabilities of the Resource
  } deriving (Eq, Generic, Show)

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
  toList x = [a, b] ++ c
    where
      a = name x
      b = fromMaybe "" $ description x
      c = A.toList $ availabilities x

-- | Generates a new 'Manifest' including a 'ResourceSpec'
--
-- @since 0.1.0
new
  :: String         -- ^ The name of the Resource
  -> Maybe String   -- ^ The description of the Resource
  -> Availabilities -- ^ The Availabilities of the Resource
  -> IO Resource    -- ^ The result
new a b c = (\m -> Manifest V1 Resource m $ ResourceSpec a b c) <$> newMetadata
