-- | This module includes everything about a times and availability
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Availability (
    Availabilities,
    Availability,
    WeekDay(..),
    available,
    availableFromTo,
    notAvailable,
    notAvailableFromTo,
    reserve,
    weeklyNotAvailable,
) where

import Data.Time.LocalTime (TimeOfDay(..))
import Data.Yaml (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M (Map, empty)
import qualified Data.Set as S ((\\), Set, empty, foldr, fromList, isSubsetOf, singleton, toList, union)

-- | The available times over a week
--
-- @since 0.1.0
newtype Availabilities = Availabilities (M.Map WeekDay Availability)
    deriving (Eq, Generic, Ord, Show)

-- | Parses the 'Availabilities' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Availabilities

-- | Generates the YAML/JSON from an 'Availabilities'
--
-- @since 0.1.0
instance ToJSON Availabilities

-- | Not available over the whole week
--
-- @since 0.1.0
weeklyNotAvailable :: Availabilities -- ^ The resulting 'Availabilities'
weeklyNotAvailable = Availabilities M.empty

-- | Possible weekdays
--
-- @since 0.1.0
data WeekDay = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
    deriving (Bounded, Eq, Enum, Generic, Ord, Show)

-- | Parses the 'WeekDay' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON WeekDay

-- | Generates the YAML/JSON from an 'WeekDay'
--
-- @since 0.1.0
instance ToJSON WeekDay

-- | Parses the 'WeekDay' Map Key from YAML/JSON
--
-- @since 0.1.0
instance FromJSONKey WeekDay

-- | Generates the YAML/JSON Map Key from an 'WeekDay'
--
-- @since 0.1.0
instance ToJSONKey WeekDay

-- | The available times over one day
--
-- @since 0.1.0
newtype Availability = Availability (S.Set TimeSpan)
    deriving (Eq, Generic, Ord, Show)

-- | Parses the 'Availability' from YAML/JSON
--
-- @since 0.1.0
instance ToJSON Availability

-- | Generates the YAML/JSON from an 'Availability'
--
-- @since 0.1.0
instance FromJSON Availability

-- | Represents the expanded form of Availability
--
-- @since 0.1.0
type ExpandedAvailability = S.Set Int

-- | Expands an 'Availability' to an 'ExpandedAvailability'
--
-- @since 0.1.0
expand
    :: Availability         -- ^ The Availability to expand
    -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
expand (Availability l) = S.foldr k S.empty l
    where k t = S.union $ toExpandedAvailability t

-- | Reduces an 'ExpandedAvailability' to an 'Availability'
--
-- @since 0.1.0
reduce
    :: ExpandedAvailability   -- ^ The 'ExpandedAvailability' to be reduced
    -> Availability           -- ^ The resulting Availability
reduce xs = Availability . S.fromList $ zipWith
    (\f t -> TimeSpan (fromMinutes f) (fromMinutes t))
    (first acc)
    (second acc)
  where
    fromMinutes m = TimeOfDay (m `quot` 60) (m `mod` 60) 0
    acc = shrink (S.toList xs) 0 []
    second (_:y:ys) = y : second ys
    second _        = []
    first (x:_:zs) = x : first zs
    first _        = []
    shrink (x:ks) _ [] = shrink ks x [x]
    shrink (x:ks) l o | m == x = shrink ks x o
                      | m /= x = shrink ks x k
      where
        m = succ l
        k = o ++ [l, x]
    shrink _ l o = o ++ [l]

-- | Gives the full daily 'Availability'
--
-- @since 0.1.0
available :: Availability -- ^ The resulting 'Availability'
available =
    Availability . S.singleton $ TimeSpan (TimeOfDay 0 0 0) (TimeOfDay 23 59 0)

-- | Gives not available for a particular day
--
-- @since 0.1.0
notAvailable :: Availability -- ^ The resulting 'Availability'
notAvailable = Availability S.empty

-- | Gives a full 'Availability' and reserves from a certain start and end time
-- 'String's. Evaluates to 'Nothing' if the Strings are not valid formatted
-- (HH:MM) or out of range.
--
-- @since 0.1.0
notAvailableFromTo
    :: String               -- ^ The start time String
    -> String               -- ^ The end time String
    -> Maybe Availability   -- ^ The resulting Availability
notAvailableFromTo f t = reserve f t available

-- | Gives 'Availability' for certain start and end time 'String's. Evaluates
-- to 'Nothing' if the Strings are not valid formatted (HH:MM) or out of range.
--
-- @since 0.1.0
availableFromTo
    :: String               -- ^ The start time String
    -> String               -- ^ The end time String
    -> Maybe Availability   -- ^ The resulting Availability
availableFromTo f t = Availability . S.singleton <$> newTimeSpan f t

-- | Tries to reserve a time interval from an 'Availability' and gives the
-- resulting 'Availability' when succeeded. Evaluates to 'Nothing' if the
-- Strings are not valid formatted (HH:MM) or out of range. Also if the time is
-- not available any more.
--
-- @since 0.1.0
reserve
    :: String               -- ^ The start time String
    -> String               -- ^ The end time String
    -> Availability         -- ^ The input Availability
    -> Maybe Availability   -- ^ The result
reserve f t a = newTimeSpan f t >>= intersect
  where
    k = expand a
    intersect x = if S.isSubsetOf i k
        then Just . reduce $ k S.\\ i
        else Nothing
        where i = toExpandedAvailability x

-- | Represents a time interval
--
-- @since 0.1.0
data TimeSpan = TimeSpan TimeOfDay TimeOfDay
    deriving (Eq, Generic, Ord, Show)

-- | Generates the YAML/JSON from an 'TimeSpan'
--
-- @since 0.1.0
instance FromJSON TimeSpan

-- | Parses the 'TimeSpan' from YAML/JSON
--
-- @since 0.1.0
instance ToJSON TimeSpan

-- | Creates a new 'TimeSpan' from two Strings in the format "HH:MM". Evaluates
-- to 'Nothing' if one of the Strings is not a readable 'Time'
--
-- @since 0.1.0
newTimeSpan
    :: String           -- ^ The start time as String
    -> String           -- ^ The end time as String
    -> Maybe TimeSpan   -- ^ The resulting TimeSpan
newTimeSpan a b
    | a <= b
    = case
            ( reads $ a ++ ":00" :: [(TimeOfDay, String)]
            , reads $ b ++ ":00" :: [(TimeOfDay, String)]
            )
        of
            ((f, _):_, (t, _):_) -> Just $ TimeSpan f t
            _                    -> Nothing
    | otherwise
    = Nothing

-- | Converts a 'TimeSpan' to a 'ExpandedAvailability'.
--
-- @since 0.1.0
toExpandedAvailability
    :: TimeSpan             -- ^ The time interval
    -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
toExpandedAvailability (TimeSpan f t) = S.fromList [toMinutes f .. toMinutes t]
    where toMinutes (TimeOfDay h m _) = 60 * h + m
