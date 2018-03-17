-- | This module includes everything about a date parsing
--
-- @since 0.1.0
--

{-# LANGUAGE FlexibleContexts #-}

module Seer.DateParser
  ( parseDate
  ) where

import Data.Char                    (isDigit
                                    ,digitToInt
                                    ,toLower
                                    ,toUpper)
import Data.List                    (isPrefixOf)
import Data.Time.Clock              (UTCTime (UTCTime
                                             ,utctDay
                                             ,utctDayTime))
import Data.Time.Calendar           (Day
                                    ,addDays
                                    ,addGregorianMonthsClip
                                    ,addGregorianYearsClip
                                    ,fromGregorian
                                    ,toGregorian)
import Data.Time.Calendar.WeekDate  (toWeekDate)
import Data.Time.LocalTime          (TimeOfDay(TimeOfDay)
                                    ,timeToTimeOfDay
                                    ,todHour
                                    ,todMin)
import Seer.Time                    (WeekDay(..))
import Text.Parsec                  (ParsecT
                                    ,Stream
                                    ,char
                                    ,choice
                                    ,digit
                                    ,letter
                                    ,many1
                                    ,notFollowedBy
                                    ,oneOf
                                    ,optionMaybe
                                    ,optional
                                    ,runParser
                                    ,space
                                    ,spaces
                                    ,string
                                    ,try
                                    ,(<|>))
import Text.Printf                  (printf)

-- | Certain interval types for relative date parsing
--
-- @since 0.1.0
data DateIntervalType = Day
                      | Week
                      | Month
                      | Year

-- | The date interval amount
--
-- @since 0.1.0
data DateInterval = Days Integer
                  | Weeks Integer
                  | Months Integer
                  | Years Integer

-- | An internal date and time representation
--
-- @since 0.1.0
data DateTime = DateTime { year   :: Int
                         , month  :: Int
                         , day    :: Int
                         , hour   :: Int
                         , minute :: Int }

-- | Shows a DateTime
--
-- @since 0.1.0
instance Show DateTime where
  show (DateTime y m d h u) = printf "%d.%d.%d - %02d:%02d" d m y h u

-- | The main parsing function. Parse from the given start date for relative
-- dates and get a resulting UTC Time or a parsing error.
--
-- @since 0.1.0
parseDate :: UTCTime -> String -> Either String UTCTime
parseDate i = do
  let (y, m, d) = toGregorian $ utctDay i
  let t         = timeToTimeOfDay $ utctDayTime i
  let dateTime  = DateTime (fromIntegral y) m d (todHour t) (todMin t)
  res <- runParser (parser dateTime) () ""
  case res of
    Left  e -> return . Left $ show e
    Right r -> return . Right $ UTCTime
      (dateTimeToDay r)
      (fromIntegral $ hour r * 60 * 60 + minute r * 60)

-- | The main parsec entry point
--
-- @since 0.1.0
parser :: Stream s m Char => DateTime -> ParsecT s st m DateTime
parser d =
  try (parseNow d) <|> try (parseRelativeDate d) <|> try (parseByWeek d) <|> try
    (parseAbsolute $ year d)

-- | Parse a relative date
--
-- @since 0.1.0
parseRelativeDate :: Stream s m Char => DateTime -> ParsecT s st m DateTime
parseRelativeDate d = do
  offs <-
    try futureDate <|> try passDate <|> try today <|> try tomorrow <|> yesterday
  parseAndSetTime (d `addInterval` offs)

parseNow :: Stream s m Char => DateTime -> ParsecT s st m DateTime
parseNow d = do
  _ <- string "now"
  return d

-- | Parse a future date, like "in 2 days"
--
-- @since 0.1.0
futureDate :: Stream s m Char => ParsecT s st m DateInterval
futureDate = do
  _       <- string "in "
  (n, tp) <- parseInterval
  case tp of
    Day   -> Days <$> tryReadInt n
    Week  -> Weeks <$> tryReadInt n
    Month -> Months <$> tryReadInt n
    Year  -> Years <$> tryReadInt n

-- | Parse an already passed date, like "2 days ago"
--
-- @since 0.1.0
passDate :: Stream s m Char => ParsecT s st m DateInterval
passDate = do
  (n, tp) <- parseInterval
  _       <- string " ago"
  case tp of
    Day   -> (Days . negate) <$> tryReadInt n
    Week  -> (Weeks . negate) <$> tryReadInt n
    Month -> (Months . negate) <$> tryReadInt n
    Year  -> (Years . negate) <$> tryReadInt n

-- | Abstract interval parsing
--
-- @since 0.1.0
parseInterval :: Stream s m Char => ParsecT s st m (String, DateIntervalType)
parseInterval = do
  n  <- many1 digit
  _  <- optional space
  tp <- pDateIntervalType
  return (n, tp)

-- | Try to read an integer from a String
--
-- @since 0.1.0
tryReadInt :: Num a => String -> ParsecT s st m a
tryReadInt str = if all isDigit str
  then return . fromIntegral . foldl (\a b -> 10 * a + b) 0 $ map digitToInt str
  else fail $ "Cannot read: " ++ str

-- | Parse today
--
-- @since 0.1.0
today :: Stream s m Char => ParsecT s st m DateInterval
today = do
  _ <- string "today"
  return $ Days 0

-- | Parse the next day
--
-- @since 0.1.0
tomorrow :: Stream s m Char => ParsecT s st m DateInterval
tomorrow = do
  _ <- string "tomorrow"
  return $ Days 1

-- | Parse yesterday
--
-- @since 0.1.0
yesterday :: Stream s m Char => ParsecT s st m DateInterval
yesterday = do
  _ <- string "yesterday"
  return $ Days (-1)

-- | Parse by a week day, like "next wednesday"
--
-- @since 0.1.0
parseByWeek :: Stream s m Char => DateTime -> ParsecT s st m DateTime
parseByWeek d = do
  i <- try (lastDate d) <|> nextDate d
  parseAndSetTime i

-- | Parse an absolute day like "5.12.2018 16:25"
--
-- @since 0.1.0
parseAbsolute :: Stream s m Char => Int -> ParsecT s st m DateTime
parseAbsolute y = do
  d <- choice $ map
    (try . ($ y))
    [const euDate, const usDate, const strDate, strDate', euDate', usDate']
  optional $ char ','
  s <- optionMaybe space
  case s of
    Nothing -> return d
    _       -> do
      t <- pTime
      return $ d `addTime` t

-- | Lookup a month for a given String
--
-- @since 0.1.0
lookupMonth :: String -> Maybe Int
lookupMonth n = lookupS n monthsN

-- | Lookup fuzzy months
--
-- @since 0.1.0
lookupS :: String -> [(String, a)] -> Maybe a
lookupS _ [] = Nothing
lookupS k ((k', v):other) | k `isPrefixOfI` k' = Just v
                          | otherwise          = lookupS k other

-- | Case insensitive prefix check
--
-- @since 0.1.0
isPrefixOfI :: String -> String -> Bool
p `isPrefixOfI` s = map toUpper p `isPrefixOf` map toUpper s

-- | Months including numbering
--
-- @since 0.1.0
monthsN :: [(String, Int)]
monthsN = zip months [1 ..]

-- | Available months
--
-- @since 0.1.0
months :: [String]
months =
  [ "january"
  , "february"
  , "march"
  , "april"
  , "may"
  , "june"
  , "july"
  , "august"
  , "september"
  , "october"
  , "november"
  , "december"
  ]

-- | Generic DateTime creation without time
--
-- @since 0.1.0
date :: Int -> Int -> Int -> DateTime
date y m d = DateTime y m d 0 0

-- | Add a Time to a DateTime
--
-- @since 0.1.0
addTime :: DateTime -> TimeOfDay -> DateTime
addTime i t = i { hour = todHour t + hour i, minute = todMin t + minute i }

-- | Set the time for a DateTime
--
-- @since 0.1.0
setTime :: DateTime -> TimeOfDay -> DateTime
setTime i t = i { hour = todHour t, minute = todMin t }

-- | Parse an date in the US format
--
-- @since 0.1.0
usDate :: Stream s m Char => ParsecT s st m DateTime
usDate = do
  y      <- pYear
  _      <- char '/'
  (d, m) <- pDayMonthTupleUs
  return $ date y m d

-- | Parse a date in the US format without year
--
-- @since 0.1.0
usDate' :: Stream s m Char => Int -> ParsecT s st m DateTime
usDate' y = do
  (d, m) <- pDayMonthTupleUs
  return $ date y m d

-- | Parse the day an month in US format
--
-- @since 0.1.0
pDayMonthTupleUs :: Stream s m Char => ParsecT s st m (Int, Int)
pDayMonthTupleUs = do
  m <- pMonth
  _ <- char '/'
  d <- pDay
  return (d, m)

-- | Parse an date in the EU format
--
-- @since 0.1.0
euDate :: Stream s m Char => ParsecT s st m DateTime
euDate = do
  (d, m) <- pDayMonthTupleEur
  _      <- char '.'
  y      <- pYear
  return $ date y m d

-- | Parse an date in the EU format without the year
--
-- @since 0.1.0
euDate' :: Stream s m Char => Int -> ParsecT s st m DateTime
euDate' y = do
  (d, m) <- pDayMonthTupleEur
  return $ date y m d

-- | Parse the day an month in EU format
--
-- @since 0.1.0
pDayMonthTupleEur :: Stream s m Char => ParsecT s st m (Int, Int)
pDayMonthTupleEur = do
  d <- pDay
  _ <- char '.'
  m <- pMonth
  return (d, m)

-- | Parse the date in the natural language format
--
-- @since 0.1.0
strDate :: Stream s m Char => ParsecT s st m DateTime
strDate = parseDayAndMonth
  ( \d m -> do
    _ <- space
    y <- pYear
    notFollowedBy $ char ':'
    return $ date y m d
  )

-- | Parse the date in the natural language format without the year
--
-- @since 0.1.0
strDate' :: Stream s m Char => Int -> ParsecT s st m DateTime
strDate' y = parseDayAndMonth (\d m -> return $ date y m d)

-- | Parse the day and month in natural language format
--
-- @since 0.1.0
parseDayAndMonth
  :: Stream s m Char => (Int -> Int -> ParsecT s st m b) -> ParsecT s st m b
parseDayAndMonth f = do
  d  <- pDay
  _  <- optional $ char '.'
  _  <- space
  ms <- many1 letter
  case lookupMonth ms of
    Nothing -> fail $ "unknown month: " ++ ms
    Just m  -> f d m

-- | Parse am/pm
--
-- @since 0.1.0
ampm :: Stream s m Char => ParsecT s st m Int
ampm = do
  s <- many1 letter
  case map toUpper s of
    "AM" -> return 0
    "PM" -> return 12
    _    -> fail "AM/PM expected"

-- | Parse the 12 hour time format
--
-- @since 0.1.0
time12 :: Stream s m Char => ParsecT s st m TimeOfDay
time12 = do
  h <- number 2 12
  _ <- char ':'
  m <- number 2 59
  optional spaces
  hd <- ampm
  return $ TimeOfDay (h + hd) m 0

-- | Parse the 24 hour time format
--
-- @since 0.1.0
time24 :: Stream s m Char => ParsecT s st m TimeOfDay
time24 = do
  h <- number 2 23
  _ <- char ':'
  m <- number 2 59
  notFollowedBy letter
  return $ TimeOfDay h m 0

-- | Convert a DateTime to a Day
--
-- @since 0.1.0
dateTimeToDay :: DateTime -> Day
dateTimeToDay i = fromGregorian (fromIntegral $ year i) (month i) (day i)

-- | Convert a Day to a DateTime
--
-- @since 0.1.0
dayToDateTime :: Day -> DateTime
dayToDateTime i = let (y, m, d) = toGregorian i in date (fromIntegral y) m d

-- | Higher order date modification
--
-- @since 0.1.0
modifyDate :: (t -> Day -> Day) -> t -> DateTime -> DateTime
modifyDate fn x i =
  let d = dayToDateTime . fn x $ dateTimeToDay i
  in  d { hour = hour i, minute = minute i }

-- | Add a valid interfacl to a DateTime
--
-- @since 0.1.0
addInterval :: DateTime -> DateInterval -> DateTime
addInterval i (Days   ds) = modifyDate addDays ds i
addInterval i (Weeks  ws) = modifyDate addDays (ws * 7) i
addInterval i (Months ms) = modifyDate addGregorianMonthsClip ms i
addInterval i (Years  ys) = modifyDate addGregorianYearsClip ys i

-- | Negate an interval
--
-- @since 0.1.0
negateInterval :: DateInterval -> DateInterval
negateInterval (Days   n) = Days (negate n)
negateInterval (Weeks  n) = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years  n) = Years (negate n)

-- | Parse a date interval type
--
-- @since 0.1.0
pDateIntervalType :: Stream s m Char => ParsecT s st m DateIntervalType
pDateIntervalType = do
  s <- choice $ map maybePlural ["day", "week", "month", "year"]
  case toLower (head s) of
    'd' -> return Day
    'w' -> return Week
    'm' -> return Month
    'y' -> return Year
    _   -> fail $ "Unknown date interval type: " ++ s

-- | Parse the plural of a String
--
-- @since 0.1.0
maybePlural :: Stream s m Char => String -> ParsecT s st m String
maybePlural str = do
  r <- string str
  optional $ char 's'
  return (capitalize r)

-- | Capitalize a String
--
-- @since 0.1.0
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

-- | Parse a date prefixed with "last"
--
-- @since 0.1.0
lastDate :: Stream s m Char => DateTime -> ParsecT s st m DateTime
lastDate now = do
  _ <- string "last"
  spaces
  try byweek <|> try bymonth <|> byyear
 where
  byweek = do
    wd <- try (string "week" >> return Mon) <|> pWeekDay
    let monday  = lastMonday now
        monday' = if wd > dateWeekDay now
          then monday `minusInterval` Weeks 1
          else monday
    return $ monday' `addInterval` weekdayToInterval wd
  bymonth = do
    _ <- string "month"
    return $ now { day = 1 }
  byyear = do
    _ <- string "year"
    return $ now { month = 1, day = 1 }

-- | Parse a date prefixed with "next"
--
-- @since 0.1.0
nextDate :: Stream s m Char => DateTime -> ParsecT s st m DateTime
nextDate now = do
  _ <- string "next"
  spaces
  try byweek <|> try bymonth <|> byyear
 where
  byweek = do
    wd <- try (string "week" >> return Mon) <|> pWeekDay
    let monday  = lastMonday now `addInterval` Weeks 1
        monday' = if wd > dateWeekDay now
          then monday `minusInterval` Weeks 1
          else monday
    return $ monday' `addInterval` weekdayToInterval wd

  bymonth = do
    _ <- string "month"
    return (now `addInterval` Months 1) { day = 1 }

  byyear = do
    _ <- string "year"
    return (now `addInterval` Years 1) { month = 1, day = 1 }

-- | Parse the last Monday
--
-- @since 0.1.0
lastMonday :: DateTime -> DateTime
lastMonday i = i `minusInterval` weekdayToInterval (dateWeekDay i)

-- | Convert a WeekDay to a DateInterval
--
-- @since 0.1.0
weekdayToInterval :: WeekDay -> DateInterval
weekdayToInterval wd = Days (fromIntegral $ fromEnum wd)

-- | Subtract a DateInterval
--
-- @since 0.1.0
minusInterval :: DateTime -> DateInterval -> DateTime
minusInterval d i = d `addInterval` negateInterval i

-- | Get the WeekDay for a DateTime
--
-- @since 0.1.0
dateWeekDay :: DateTime -> WeekDay
dateWeekDay i = let (_, _, wd) = toWeekDate (dateTimeToDay i) in toEnum wd

-- | Parse available WeekDay's
--
-- @since 0.1.0
pWeekDay :: Stream s m Char => ParsecT s st m WeekDay
pWeekDay = do
  w <- many1 (oneOf "mondaytueswnhrfi")
  case map toLower w of
    x | "mo" `isPrefixOfI` x -> return Mon
      | "tu" `isPrefixOfI` x -> return Tue
      | "we" `isPrefixOfI` x -> return Wed
      | "th" `isPrefixOfI` x -> return Thu
      | "fr" `isPrefixOfI` x -> return Fri
      | "sa" `isPrefixOfI` x -> return Sat
      | "su" `isPrefixOfI` x -> return Sun
    _ -> fail $ "Unknown weekday: " ++ w

-- | Parse and set a time
--
-- @since 0.1.0
parseAndSetTime :: Stream s m Char => DateTime -> ParsecT s u m DateTime
parseAndSetTime d = do
  optional $ char ','
  s <- optionMaybe space
  case s of
    Nothing -> return $ date (year d) (month d) (day d)
    _       -> do
      t <- pTime
      return $ d `setTime` t

-- | Parse a time in 12 or 24 hour format
--
-- @since 0.1.0
pTime :: Stream s m Char => ParsecT s st m TimeOfDay
pTime = choice $ map try [time12, time24]

-- | Parse the year
--
-- @since 0.1.0
pYear :: Stream s m Char => ParsecT s st m Int
pYear = do
  y <- number 4 10000
  if y < 2000 then return (y + 2000) else return y

-- | Parse the month
--
-- @since 0.1.0
pMonth :: Stream s m Char => ParsecT s st m Int
pMonth = number 2 12

-- | Parse the day
--
-- @since 0.1.0
pDay :: Stream s m Char => ParsecT s st m Int
pDay = number 2 31

-- | Parse a generic number
--
-- @since 0.1.0
number
  :: Stream s m Char
  => Int                -- ^ Number of digits
  -> Int                -- ^ Maximum value
  -> ParsecT s st m Int -- ^ The result
number n m = do
  t <- tryReadInt =<< (n `times` digit)
  if t > m then fail "number too large" else return t

-- | Parse multiple times
--
-- @since 0.1.0
times :: (Stream s m Char) => Int -> ParsecT s st m t -> ParsecT s st m [t]
times 0 _ = return []
times n p = do
  ts <- times (n - 1) p
  t  <- optionMaybe p
  case t of
    Just t' -> return (ts ++ [t'])
    Nothing -> return ts
