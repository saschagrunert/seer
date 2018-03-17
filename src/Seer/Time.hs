-- | This module includes everything about a times and availability
--
-- @since 0.1.0

{-# LANGUAGE DeriveGeneric   #-}

module Seer.Time
  ( Availabilities
  , Availability
  , Duration(Duration)
  , MonadTime
  , TimeSpanString
  , WeekDay(..)
  , dayAvailable
  , dayAvailableFromTo
  , dayNotAvailable
  , dayNotAvailableFromTo
  , dayReserveTime
  , dateTimeFormat
  , evaluateEnd
  , evaluateStart
  , parseDuration
  , toList
  , utcToLocal
  , weekAvailable
  , weekAvailableFromTo
  , weekNotAvailable
  , weekNotAvailableFromTo
  , weekReserveTime
  ) where

import           Control.Monad        (foldM
                                      ,mapM)
import           Data.Aeson.Types     (FromJSONKey
                                      ,ToJSONKey)
import           Data.Char            (isDigit)
import           Data.List            (intercalate)
import           Data.List.Split      (splitOn)
import qualified Data.Map.Strict as M (Map
                                      ,adjust
                                      ,lookup
                                      ,fromList
                                      ,singleton
                                      ,toList
                                      ,union
                                      ,unionsWith)
import qualified Data.Set as S        (Set
                                      ,empty
                                      ,fromList
                                      ,isSubsetOf
                                      ,singleton
                                      ,toList
                                      ,union
                                      ,unions
                                      ,(\\))
import           Data.Time.Calendar   (addDays
                                      ,toModifiedJulianDay)
import           Data.Time.Clock      (UTCTime(UTCTime)
                                      ,addUTCTime
                                      ,utctDay
                                      ,utctDayTime)
import           Data.Time.Format     (FormatTime
                                      ,defaultTimeLocale
                                      ,formatTime)
import           Data.Time.LocalTime  (TimeOfDay (TimeOfDay)
                                      ,TimeZone
                                      ,getCurrentTimeZone
                                      ,todHour
                                      ,todMin
                                      ,utcToLocalTime)
import           Data.Yaml            (FromJSON
                                      ,ToJSON)
import           GHC.Generics         (Generic)
import           Seer.Manifest        (ToList (headers
                                              ,toList))
import           Seer.Utils           (rstrip)
import           Text.Printf          (printf)

-- | A abstraction Monad to isolate real IO Actions
--
-- @since 0.1.0
class Monad m => MonadTime m where
  getCurrentTimeZone' :: m TimeZone

-- | The implementation of the isolation abstraction for the IO Monad
--
-- @since 0.1.0
instance MonadTime IO where
  getCurrentTimeZone' = getCurrentTimeZone

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
  show (Availabilities a) = intercalate "; " $ (\(o, t) -> show o ++ ": " ++ show t) <$> k
    where k = filter (\(_, x) -> x /= dayNotAvailable) $ M.toList a

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
  deriving (Eq, Generic, Ord, Show)

-- | Enum representation for 'WeekDay's
--
-- @since 0.1.0
instance Enum WeekDay where
  toEnum i = case mod i 7 of
    0 -> Sun
    1 -> Mon
    2 -> Tue
    3 -> Wed
    4 -> Thu
    5 -> Fri
    _ -> Sat
  fromEnum i = case i of
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6
    Sun -> 7

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
  headers _ = show <$> [Mon .. Sun]
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
  :: Availability   -- ^ The given 'Availability'
  -> Availabilities -- ^ The resulting 'Availabilities'
toAvailabilities a = Availabilities $ M.fromList [ (x, a) | x <- [Mon .. Sun] ]

-- | Expresses 'Availabilities' for certain 'WeekDay's, start and end time
-- 'String's. Evaluates to 'Nothing' if the Strings are not valid formatted
-- (HH:MM) or out of range. Multiple time ranges for a single day will be
-- merged together.
--
-- @since 0.1.0
weekAvailableFromTo
  :: [(WeekDay, TimeSpanString)] -- ^ The availability day, start and end time String
  -> Maybe Availabilities        -- ^ The resulting 'Availabilities'
weekAvailableFromTo i =
  mergeAvailabilities weekNotAvailable
    .   Availabilities
    .   M.unionsWith mergeAvailability
    <$> mapM toWeekDayA i
  where toWeekDayA (x, t) = M.singleton x <$> dayAvailableFromTo [t]

-- | Expresses the non availability by time inversion. Evaluates to 'Nothing'
-- if the Strings are not valid formatted (HH:MM) or out of range.
--
-- @since 0.1.0
weekNotAvailableFromTo
  :: [(WeekDay, TimeSpanString)] -- ^ The not available day, start and end time String
  -> Maybe Availabilities        -- ^ The resulting 'Availabilities'
weekNotAvailableFromTo = foldM (flip weekReserveTime) weekAvailable

-- | Tries to reserve a time interval from a 'WeekDay' and 'Availabilities' and
-- gives the resulting 'Availabilities' when succeeded. Evaluates to 'Nothing'
-- if the Strings are not valid formatted (HH:MM), out of range or if the time
-- slot is not available any more.
--
-- @since 0.1.0
weekReserveTime
  :: (WeekDay, TimeSpanString) -- ^ The reservation day and time
  -> Availabilities            -- ^ The input Availabilities
  -> Maybe Availabilities      -- ^ The resulting 'Availabilities'
weekReserveTime (e, t) (Availabilities a) =
  M.lookup e a
    >>= dayReserveTime t
    >>= (\x -> return . Availabilities $ M.adjust (const x) e a)

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
  :: Availability         -- ^ The Availability to expand
  -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
expand (Availability l) = foldr k S.empty l
  where k t = S.union $ toExpandedAvailability t

-- | Reduces an 'ExpandedAvailability' to an 'Availability'
--
-- @since 0.1.0
reduce
  :: ExpandedAvailability -- ^ The 'ExpandedAvailability' to be reduced
  -> Availability         -- ^ The resulting Availability
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
  :: [TimeSpanString]   -- ^ The start and end time String
  -> Maybe Availability -- ^ The resulting Availability
dayAvailableFromTo t = Availability . S.fromList <$> newTimeSpans t

-- | Expresses the availability by time inversion. Evaluates to 'Nothing' if
-- the Strings are not valid formatted (HH:MM) or out of range.
--
-- @since 0.1.0
dayNotAvailableFromTo
  :: [TimeSpanString]   -- ^ The start and end time Strings
  -> Maybe Availability -- ^ The resulting Availability
dayNotAvailableFromTo t = newTimeSpans t >>= foldM dayReserve dayAvailable

-- | Tries to reserve a time interval from an 'Availability' and gives the
-- resulting 'Availability' when succeeded. Evaluates to 'Nothing' if the
-- Strings are not valid formatted (HH:MM) or out of range. Also if the time
-- slot is not available any more.
--
-- @since 0.1.0
dayReserveTime
  :: TimeSpanString     -- ^ The start and end time String
  -> Availability       -- ^ The input Availability
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
  show (TimeSpan a b) = printf "%d:%02d-%d:%02d" ha ma hb mb
    where ha = todHour a
          ma = todMin a
          hb = todHour $ po b
          mb = todMin $ po b
          po (TimeOfDay 23 59 _) = TimeOfDay 0 0 0
          po (TimeOfDay x 59 _) = TimeOfDay (x + 1) 0 0
          po (TimeOfDay x z _) = TimeOfDay x (z + 1) 0

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
  p (a:b:_) =
    case
        ( reads $ n a :: [(TimeOfDay, String)]
        , reads $ n b :: [(TimeOfDay, String)]
        )
      of
        ((f, _):_, (t, _):_) | f <= mo t -> Just . TimeSpan f $ mo t
        _ -> Nothing
  p _ = Nothing
  n a = case splitOn ":" a of
    (o:m:_) -> printf "%02s:%02s:00" o m
    (o  :_) -> printf "%02s:00:00" o
    _       -> ""
  mo (TimeOfDay 0 0 _) = TimeOfDay 23 59 0
  mo (TimeOfDay x 0 _) = TimeOfDay (x - 1) 59 0
  mo (TimeOfDay x z _) = TimeOfDay x (z - 1) 0

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
  :: TimeSpan             -- ^ The time interval
  -> ExpandedAvailability -- ^ The resulting ExpandedAvailability
toExpandedAvailability (TimeSpan f t) = S.fromList [toMinutes f .. toMinutes t]
  where toMinutes (TimeOfDay o m _) = 60 * o + m

-- | Converts an 'ExpandedAvailability' to a list of 'TimeSpan'
--
-- @since 0.1.0
toTimeSpan
  :: ExpandedAvailability -- ^ The 'ExpandedAvailability' to be reduced
  -> [TimeSpan]           -- ^ The 'TimeSpan's
toTimeSpan xs = zipWith (\f t -> TimeSpan (fromMinutes f) (fromMinutes t))
                        (first acc)
                        (second acc)
 where
  fromMinutes m = TimeOfDay (m `quot` 60) (m `mod` 60) 0
  acc = shrink (S.toList xs) 0 []
  second (_:o:os) = o : second os
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

-- | Representation of a Duration
--
-- @since 0.1.0
newtype Duration = Duration Int
  deriving (Eq, Generic, Ord)

-- | Shows a 'Duration'
--
-- @since 0.1.0
instance Show Duration where
  show (Duration m)
    | m >= y = go y "y "
    | m >= w = go w "w "
    | m >= d = go d "d "
    | m >= h = go h "h "
    | m < h && m > 0 = show m ++ "m"
    | otherwise = ""
    where go x s = rstrip $ showQuot x s ++ next x
          next x = show . Duration $ m `mod` x
          showQuot x = (++) . show $ m `quot` x

-- | Reads a 'Duration'
--
-- @since 0.1.0
instance Read Duration where
  readsPrec _ str = maybe [] (pure . flip (,) "") $ parseDuration str

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
parseDuration
  :: String         -- ^ The input duration string
  -> Maybe Duration -- ^ The result
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
  units = [('y', y), ('w', w), ('d', d), ('h', h), ('m', 1)]

-- | Representation of one year
--
-- @since 0.1.0
y :: Int
y = d * 365

-- | Representation of one week
--
-- @since 0.1.0
w :: Int
w = d * 7

-- | Representation of one day
--
-- @since 0.1.0
d :: Int
d = h * 24

-- | Representation of one hour
--
-- @since 0.1.0
h :: Int
h = 60

-- | Convert an UTC time to the real local time
--
-- @since 0.1.0
utcToLocal :: MonadTime m => UTCTime -> m String
utcToLocal t = dateTimeFormat . flip utcToLocalTime t <$> getCurrentTimeZone'

-- | The default date formatting output
--
-- @since 0.1.0
dateTimeFormat :: FormatTime a => a -> String
dateTimeFormat = formatTime defaultTimeLocale "%d.%m.%y %k:%M"

-- | Get the start UTCTime for a proposed start and availabilities
--
-- @since 0.1.0
evaluateStart
  :: UTCTime        -- ^ The proposed start time
  -> Availabilities -- ^ The Availabilities of the Resource
  -> Maybe UTCTime  -- ^ The resulting start time
evaluateStart _ v | v == weekNotAvailable = Nothing
evaluateStart t v                         = case getAvailabilityForDate t v of
  Just x -> case startTime t x of
    Just (r, _) -> Just r
    Nothing     -> evaluateNextDay
  Nothing -> evaluateNextDay
  where evaluateNextDay = evaluateStart (nextDay t) v

-- | Get the resulting UTCTime for a given start, availabilities and duration
--
-- @since 0.1.0
evaluateEnd
  :: UTCTime        -- ^ The start time
  -> Availabilities -- ^ The Availabilities of the Resource
  -> Duration       -- ^ The duration of the Action
  -> Maybe UTCTime  -- ^ The resulting end time
evaluateEnd _ v _ | v == weekNotAvailable = Nothing
evaluateEnd t v k                         = case getAvailabilityForDate t v of
  Just x -> case consumeDuration t x k of
    Just (r, Duration 0) -> Just r
    Just (r, n         ) -> evaluateEnd (nextDay r) v n
    Nothing              -> evaluateNextDay
  Nothing -> evaluateNextDay
  where evaluateNextDay = evaluateEnd (nextDay t) v k

-- | Get the next UTC day beggining from second 0
--
-- @since 0.1.0
nextDay :: UTCTime -> UTCTime
nextDay x = UTCTime (addDays 1 $ utctDay x) 0

-- | Get the 'Availabilities' for a certain 'UTCTime'
--
-- @since 0.1.0
getAvailabilityForDate :: UTCTime -> Availabilities -> Maybe Availability
getAvailabilityForDate t = getAvailability $ dayOfWeek t
 where
  dayOfWeek x = toEnum . fromInteger $ toModifiedJulianDay (utctDay x) + 3
  getAvailability a (Availabilities b) = case M.lookup a b of
    Just r | r == dayNotAvailable -> Nothing
           | otherwise            -> Just r
    Nothing -> Nothing

-- | Tries to consume the given duration with the input Availability. The
-- result is a new time and duration, depending on the availability.
--
-- @since 0.1.0
consumeDuration
  :: UTCTime                   -- ^ The starting Time
  -> Availability              -- ^ The availability to be used
  -> Duration                  -- ^ The duration to be consumed
  -> Maybe (UTCTime, Duration) -- ^ The resulting Time and Duration
consumeDuration t a k = (\(x, z) -> foldl c (x, k) z) <$> startTime t a
 where
  c (u, Duration o) _ | o == 0    = (u, Duration 0)
                      | otherwise = (addUTCTime 60 u, Duration $ pred o)

-- | Retrieve the start UTCTime and the residual minutes for a given
-- Availability. Results in none if no 'Availability' is left for this day.
--
-- @since 0.1.0
startTime :: UTCTime -> Availability -> Maybe (UTCTime, [Int])
startTime t a =
  case dropWhile (\x -> utctDayTime t > toDiffTime x) (S.toList $ expand a) of
    []      -> Nothing
    l@(x:_) -> Just (UTCTime (utctDay t) $ toDiffTime x, l)
  where toDiffTime x = fromIntegral $ x * 60
