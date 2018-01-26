-- | This module includes everything about a 'Action'.
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Entity.Action (
    ActionSpec(..),
    new,
) where

import Data.Char (isDigit)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Seer.Entity.Manifest (ApiVersion(V1)
                            ,Manifest(Manifest)
                            ,ResourceKind(Action)
                            ,newMetadata)
import qualified Data.Map as M (fromList, lookup)

-- | The data specified for a Action
--
-- @since 0.1.0
data ActionSpec = ActionSpec { name :: String              -- ^ The name of the Action
                             , description :: Maybe String -- ^ The general description of the Action
                             , duration :: Duration        -- ^ The duration of the Action
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
    :: String                           -- ^ The name of the Action
    -> Maybe String                     -- ^ The description of the Action
    -> String                           -- ^ The duration of the Action
    -> IO (Maybe (Manifest ActionSpec)) -- ^ The result
new a b c =
    (\m -> (Manifest V1 Action m . ActionSpec a b) <$> parseDuration c)
        <$> newMetadata

-- | Generates the YAML/JSON from an 'ActionSpec'
--
-- @since 0.1.0
newtype Duration = Duration Integer
    deriving (Eq, Generic, Show)

-- | Parses the 'Duration' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Duration

-- | Generates the YAML/JSON from an 'Duration'
--
-- @since 0.1.0
instance ToJSON Duration

-- | Parse a 'Duration' from a 'String' with the format "5m" or "1h50m".
-- Evaluates to Nothing if the format is incorrect.
--
-- @since 0.1.0
parseDuration :: String -> Maybe Duration
parseDuration = f 0
  where
    f n [] = Just $ Duration n
    f n s  = do
        num <- readish s
        case dropWhile isDigit (strip s) of
            (c:rest) -> do
                u <- M.lookup c $ M.fromList units
                f (n + num * u) rest
            _ -> Just . Duration $ n + num
    readish s = case reads s of
        ((x, _):_) -> Just x
        _          -> Nothing
    strip = filter (/= ' ')
    units = [('y', ysecs), ('d', dsecs), ('h', hsecs), ('m', msecs)]
      where
        ysecs = dsecs * 365
        dsecs = hsecs * 24
        hsecs = 60
        msecs = 1
