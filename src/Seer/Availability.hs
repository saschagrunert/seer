-- | This module includes everything about a times and availability
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric #-}

module Seer.Availability
  ( Availabilities
  , Availability
  , TimeSpanString
  , WeekDay(..)
  , dayAvailable
  , dayAvailableFromTo
  , dayNotAvailable
  , dayNotAvailableFromTo
  , dayReserveTime
  , toList
  , weekAvailable
  , weekAvailableFromTo
  , weekNotAvailable
  , weekNotAvailableFromTo
  , weekReserveTime
  ) where

import           Control.Monad       (foldM, mapM)
import           Data.Aeson.Types    (FromJSONKey, ToJSONKey)
import           Data.List           (intercalate)
import           Data.List.Split     (splitOn)
import qualified Data.Map.Strict     as M (Map, adjust, fromList, lookup,
                                           singleton, toList, union, unionsWith)
import qualified Data.Set            as S (Set, empty, foldr, fromList,
                                           isSubsetOf, singleton, toList, union,
                                           unions, (\\))
import           Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import           Data.Yaml           (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Seer.Manifest       (ToList (toList))

-- | The available times over a week
--
-- @since 0.1.0
newtype Availabilities =
  Availabilities (M.Map WeekDay Availability)
  deriving (Eq, Generic, Ord)

-- | Parses the 'Availabilities' from YAML/JSON
--
-- @since 0.1.0
instance FromJSON Availabilities

-- | Generates the YAML/JSON from an 'Availabilities'
--
-- @since 0.1.0
instance ToJSON Availabilities

-- | Shows the 'Availabilities'
--
-- @since 0.1.0
instance Show Availabilities where
  show (Availabilities a) = intercalate "; " $ (\(w, t) -> show w ++ ": " ++ show t) <$> k
    where k = filter (\(_, d) -> d /= dayNotAvailable) $ M.toList a

-- | Possible weekdays
--
-- @since 0.1.0
data WeekDay
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Eq, Enum, Generic, Ord, Show)

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

-- | Encode an `Availabilities` as a list of Strings
--
-- @since 0.1.0
instance ToList Availabilities where
  toList (Availabilities a) = (\(_, t) -> show t) <$> M.toList a

-- | Merges two 'Availabilities' into a single one, prefers the right one when
-- duplicate entries are encountered.
--
-- @since 0.1.0
mergeAvailabilities
  :: Availabilities -- ^ The first 'Availabilities' to be merged
  -> Availabilities -- ^ The second 'Availabilities' to be merged
  -> Availabilities -- ^ The resulting 'Availabilities'
mergeAvailabilities (Availabilities l) (Availabilities r) =
  Availabilities $ M.union r l

-- | Expresses full availability of the whole week
--
-- @since 0.1.0
weekAvailable :: Availabilities -- ^ The resulting 'Availabilities'
weekAvailable = toAvailabilities dayAvailable

-- | Expresses not available over the whole week
--
-- @since 0.1.0
weekNotAvailable :: Availabilities -- ^ The resulting 'Availabilities'
weekNotAvailable = toAvailabilities dayNotAvailable

-- | Expresses a weekly repeating 'Availabilities' for a given daily 'Availability'
--
-- @since 0.1.0
toAvailabilities
  :: Availability -- ^ The given 'Availability'
  -> Availabilities -- ^ The resulting 'Availabilities'
toAvailabilities a = Availabilities $ M.fromList [ (w, a) | w <- [Mon .. Sun] ]

-- | Expresses 'Availabilities' for certain 'WeekDay's, start and end time
-- 'String's. Evaluates to 'Nothing' if the Strings are not valid formatted
-- (HH:MM) or out of range. Multiple time ranges for a single day will be
-- merged together.
--
-- @since 0.1.0
weekAvailableFromTo
  :: [(WeekDay, TimeSpanString)] -- ^ The availability day, start and end time String
  -> Maybe Availabilities -- ^ The resulting 'Availabilities'
weekAvailableFromTo i =
  mergeAvailabilities weekNotAvailable
    .   Availabilities
    .   M.unionsWith mergeAvailability
    <$> mapM toWeekDayA i
  where toWeekDayA (w, t) = M.singleton w <$> dayAvailableFromTo [t]

-- | Expresses the non availability by time inversion. Evaluates to 'Nothing'
-- if the Strings are not valid formatted (HH:MM) or out of range.
--
-- @since 0.1.0
weekNotAvailableFromTo
  :: [(WeekDay, TimeSpanString)] -- ^ The not available day, start and end time String
  -> Maybe Availabilities -- ^ The resulting 'Availabilities'
weekNotAvailableFromTo = foldM (flip weekReserveTime) weekAvailable

-- | Tries to reserve a time interval from a 'WeekDay' and 'Availabilities' and
-- gives the resulting 'Availabilities' when succeeded. Evaluates to 'Nothing'
-- if the Strings are not valid formatted (HH:MM), out of range or if the time
-- slot is not available any more.
--
-- @since 0.1.0
weekReserveTime
  :: (WeekDay, TimeSpanString) -- ^ The reservation day and time
  -> Availabilities -- ^ The input Availabilities
  -> Maybe Availabilities -- ^ The resulting 'Availabilities'
weekReserveTime (w, t) (Availabilities a) =
  M.lookup w a
    >>= dayReserveTime t
    >>= (\x -> return . Availabilities $ M.adjust (const x) w a)

-- | The available times over one day
--
-- @since 0.1.0
newtype Availability =
  Availability (S.Set TimeSpan)
  deriving (Eq, Generic, Ord)

-- | Parses the 'Availability' from YAML/JSON
--
-- @since 0.1.0
instance ToJSON Availability

-- | Generates the YAML/JSON from an 'Availability'
--
-- @since 0.1.0
instance FromJSON Availability

-- | Shows the 'Availability'
--
-- @since 0.1.0
instance Show Availability where
  show (Availability a) = intercalate ", " $ show <$> S.toList a

-- | Represents the expanded form of Availability
--
-- @since 0.1.0
type ExpandedAvailability = S.Set Int

-- | Merges two 'Availability' into a single one
--
-- @since 0.1.0
mergeAvailability
  :: Availability -- ^ The first 'Availability' to be merged
  -> Availability -- ^ The second 'Availability' to be merged
  -> Availability -- ^ The resulting 'Availability'
mergeAvailability l r = reduce $ S.union (expand l) (expand r)

-- | Expands an 'Availability' to an 'ExpandedAvailability'
--
-- @since 0.1.0
expand
  :: Availability -- ^ The Availability to expand
  -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
expand (Availability l) = S.foldr k S.empty l
  where k t = S.union $ toExpandedAvailability t

-- | Reduces an 'ExpandedAvailability' to an 'Availability'
--
-- @since 0.1.0
reduce
  :: ExpandedAvailability -- ^ The 'ExpandedAvailability' to be reduced
  -> Availability -- ^ The resulting Availability
reduce = Availability . S.fromList . toTimeSpan

-- | Expresses the full daily 'Availability'
--
-- @since 0.1.0
dayAvailable :: Availability -- ^ The resulting 'Availability'
dayAvailable =
  Availability . S.singleton $ TimeSpan (TimeOfDay 0 0 0) (TimeOfDay 23 59 0)

-- | Expresses not available for a particular day
--
-- @since 0.1.0
dayNotAvailable :: Availability -- ^ The resulting 'Availability'
dayNotAvailable = Availability S.empty

-- | Expresses 'Availability' for certain start and end time 'String's. Evaluates
-- to 'Nothing' if the Strings are not valid formatted (HH:MM) or out of range.
--
-- @since 0.1.0
dayAvailableFromTo
  :: [TimeSpanString] -- ^ The start and end time String
  -> Maybe Availability -- ^ The resulting Availability
dayAvailableFromTo t = Availability . S.fromList <$> newTimeSpans t

-- | Expresses the availability by time inversion. Evaluates to 'Nothing' if
-- the Strings are not valid formatted (HH:MM) or out of range.
--
-- @since 0.1.0
dayNotAvailableFromTo
  :: [TimeSpanString] -- ^ The start and end time Strings
  -> Maybe Availability -- ^ The resulting Availability
dayNotAvailableFromTo t = newTimeSpans t >>= foldM dayReserve dayAvailable

-- | Tries to reserve a time interval from an 'Availability' and gives the
-- resulting 'Availability' when succeeded. Evaluates to 'Nothing' if the
-- Strings are not valid formatted (HH:MM) or out of range. Also if the time
-- slot is not available any more.
--
-- @since 0.1.0
dayReserveTime
  :: TimeSpanString -- ^ The start and end time String
  -> Availability -- ^ The input Availability
  -> Maybe Availability -- ^ The result
dayReserveTime t a = newTimeSpan t >>= dayReserve a

-- | Tries to reserve a time interval from an 'Availability' and gives the
-- resulting 'Availability' when succeeded. Evaluates to 'Nothing' if the
-- time slot is not avialable any more.
--
-- @since 0.1.0
dayReserve :: Availability -> TimeSpan -> Maybe Availability
dayReserve a x = if S.isSubsetOf i k then Just . reduce $ k S.\\ i else Nothing
 where
  i = toExpandedAvailability x
  k = expand a

-- | Represents a time interval
--
-- @since 0.1.0
data TimeSpan =
  TimeSpan TimeOfDay
           TimeOfDay
  deriving (Eq, Generic, Ord)

-- | Generates the YAML/JSON from an 'TimeSpan'
--
-- @since 0.1.0
instance FromJSON TimeSpan

-- | Parses the 'TimeSpan' from YAML/JSON
--
-- @since 0.1.0
instance ToJSON TimeSpan

-- | Shows the 'TimeSpan'
--
-- @since 0.1.0
instance Show TimeSpan where
  show (TimeSpan a b) = take 5 (show a) ++ "-" ++ take 5 (show b)

-- | Represents the String form of a TimeSpan with the format: "HH:MM-HH:MM"
--
-- @since 0.1.0
type TimeSpanString = String

-- | Creates a new 'TimeSpan' from String in the format "HH:MM-HH:MM".
-- Evaluates to 'Nothing' if one of the Strings is not a readable 'Time'
--
-- @since 0.1.0
newTimeSpan :: TimeSpanString -> Maybe TimeSpan
newTimeSpan s = p $ splitOn "-" s
 where
  p (a:b:_) | a <= b =
    case
        ( reads $ a ++ ":00" :: [(TimeOfDay, String)]
        , reads $ b ++ ":00" :: [(TimeOfDay, String)]
        )
      of
        ((f, _):_, (t, _):_) -> Just $ TimeSpan f t
        _                    -> Nothing
  p _ = Nothing

-- | Creates a list of 'TimeSpan' from a list of Strings. Evaluates to
-- 'Nothing' if one of the Strings is not a readable 'Time'
--
-- @since 0.1.0
newTimeSpans
  :: [TimeSpanString] -- ^ The input list of TimeSpan Strings
  -> Maybe [TimeSpan] -- ^ The resulting list of 'TimeSpan's
newTimeSpans x = mergeTimeSpans <$> mapM newTimeSpan x

-- | Converts a 'TimeSpan' to a 'ExpandedAvailability'.
--
-- @since 0.1.0
toExpandedAvailability
  :: TimeSpan -- ^ The time interval
  -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
toExpandedAvailability (TimeSpan f t) = S.fromList [toMinutes f .. toMinutes t]
  where toMinutes (TimeOfDay h m _) = 60 * h + m

-- | Converts an 'ExpandedAvailability' to a list of 'TimeSpan'
--
-- @since 0.1.0
toTimeSpan
  :: ExpandedAvailability -- ^ The 'ExpandedAvailability' to be reduced
  -> [TimeSpan] -- ^ The 'TimeSpan's
toTimeSpan xs = zipWith (\f t -> TimeSpan (fromMinutes f) (fromMinutes t))
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

-- | Merges multiple overlapping 'TimeSpan' into dedicated ones
--
-- @since 0.1.0
mergeTimeSpans
  :: [TimeSpan] -- ^ The input 'TimeSpan's
  -> [TimeSpan] -- ^ The resulting 'TimeSpan's
mergeTimeSpans = toTimeSpan . S.unions . map toExpandedAvailability
