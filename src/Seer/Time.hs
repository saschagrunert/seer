-- | This module includes everything about a times and availability
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Time (
    Availabilities,
    Availability,
    TimeSpan,
    WeekDay(..),
    fullAvailable,
    newTime,
    newTimeSpan,
    notAvailable,
    notAvailableAtAll,
    reserve,
) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Yaml (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import GHC.Generics (Generic)
import Text.Printf (printf)
import qualified Data.Map.Strict as M (Map, empty)
import qualified Data.Set as S ((\\), Set, empty, foldr, fromList, isSubsetOf, singleton, toList)

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
notAvailableAtAll :: Availabilities -- ^ The resulting 'Availabilities'
notAvailableAtAll = Availabilities M.empty

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
-- @since 0.w.0
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
type ExpandedAvailability = S.Set Minutes

-- | Represents minutes
--
-- @since 0.1.0
type Minutes = Int

-- | Represents hours
--
-- @since 0.1.0
type Hours = Int

-- | Expands an 'Availability' to an 'ExpandedAvailability'
--
-- @since 0.1.0
expand
    :: Availability         -- ^ The Availability to expand
    -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
expand (Availability l) = S.fromList $ S.foldr k [] l
    where k (TimeSpan f t) = (++) [toMinutes f .. toMinutes t]

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

-- | Gives the fullAvailable 'Availability'
--
-- @since 0.1.0
fullAvailable :: Availability -- ^ The resulting fullAvailable 'Availability'
fullAvailable =
    Availability . S.singleton $ TimeSpan (TimeOfDay 0 0 0) (TimeOfDay 23 59 0)

-- | not available for a particular day
--
-- @since 0.1.0
notAvailable :: Availability -- ^ The resulting 'Availability'
notAvailable = Availability S.empty

-- | Tries to reserve a time interval from an 'Availability' and gives the
-- resulting 'Availability' when succeeded
--
-- @since 0.1.0
reserve
    :: Availability         -- ^ The input Availability
    -> TimeSpan             -- ^ The time interval
    -> Maybe Availability   -- ^ The result
reserve a t = if S.isSubsetOf i k then Just . reduce $ k S.\\ i else Nothing
  where
    i = toExpandedAvailability t
    k = expand a

-- | Represents a time interval
--
-- @since 0.1.0
data TimeSpan = TimeSpan { from :: TimeOfDay -- ^ The starting time
                         , to :: TimeOfDay   -- ^ The ending time
                         } deriving (Eq, Generic, Ord, Show)

-- | Generates the YAML/JSON from an 'TimeSpan'
--
-- @since 0.1.0
instance FromJSON TimeSpan

-- | Parses the 'TimeSpan' from YAML/JSON
--
-- @since 0.1.0
instance ToJSON TimeSpan

-- | Creates a new 'TimeSpan' from two Strings. Evaluates to 'Nothing' if one
-- of the Strings is not a readable 'Time'
--
-- @since 0.1.0
newTimeSpan
    :: String           -- ^ The start time as String
    -> String           -- ^ The end time as String
    -> Maybe TimeSpan   -- ^ The resulting TimeSpan
newTimeSpan a b
    | a <= b = case (reads a, reads b) of
        ([]      , _       ) -> Nothing
        (_       , []      ) -> Nothing
        ((f, _):_, (t, _):_) -> Just TimeSpan {from = f, to = t}
    | otherwise = Nothing

-- | Converts a 'TimeSpan' to a 'ExpandedAvailability'.
--
-- @since 0.1.0
toExpandedAvailability
    :: TimeSpan             -- ^ The time interval
    -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
toExpandedAvailability (TimeSpan f t) = S.fromList [toMinutes f .. toMinutes t]

-- | Creates a new Time from Hours and Minutes. Evaluates to 'Nothing' if the
-- Hours and Minutes Range are not within 0-23 (hours) and 0-59 (minutes).
--
-- @since 0.1.0
newTime
    :: Hours           -- ^ The Hours
    -> Minutes         -- ^ The Minutes
    -> Maybe TimeOfDay -- ^ The resulting Time
newTime h m | between 0 23 h && between 0 59 m = Just $ TimeOfDay h m 0
            | otherwise                        = Nothing
    where between low high val = low <= val && val <= high

-- | Converts a 'TimeOfDay' to the corresponding minutes per day
--
-- @since 0.1.0
toMinutes
    :: TimeOfDay -- ^ The actual 'TimeOfDay'
    -> Minutes   -- ^ The resulting Minutes
toMinutes (TimeOfDay h m _) = 60 * h + m

-- | Converts minutes to a 'TimeOfDay'
--
-- @since 0.1.0
fromMinutes
    :: Minutes   -- ^ The Minutes
    -> TimeOfDay -- ^ The resulting 'TimeOfDay'
fromMinutes m = TimeOfDay (m `quot` 60) (m `mod` 60) 0
