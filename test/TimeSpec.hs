-- | The Time tests
--
-- @since 0.1.0

module TimeSpec
  ( timeProps
  , timeSpec
  ) where

import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.ByteString.Char8        (pack)
import Data.Maybe                   (fromJust
                                    ,isJust)
import Data.Time.Calendar           (fromGregorian)
import Data.Time.Clock              (UTCTime(UTCTime))
import Data.Time.LocalTime          (minutesToTimeZone)
import Data.Yaml                    (decode
                                    ,encode)
import Seer.Time                    (Duration(Duration)
                                    ,MonadTime
                                    ,WeekDay(..)
                                    ,dayAvailable
                                    ,dayAvailableFromTo
                                    ,dayNotAvailable
                                    ,dayNotAvailableFromTo
                                    ,dayReserveTime
                                    ,dateTimeFormat
                                    ,evaluateDaily
                                    ,evaluateEnd
                                    ,evaluateStart
                                    ,toList
                                    ,utcToLocal
                                    ,weekAvailable
                                    ,weekAvailableFromTo
                                    ,weekNotAvailable
                                    ,weekNotAvailableFromTo
                                    ,weekReserveTime)
import Test.Tasty                   (TestTree
                                    ,testGroup)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import Test.Tasty.QuickCheck        (Arbitrary(..)
                                    ,elements
                                    ,testProperty
                                    ,(==>))
import TestData                     (testTime)

mkFixture "Fixture" [ts| MonadTime |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _getCurrentTimeZone' = return $ minutesToTimeZone 0 }

-- Property generators
newtype TimeSpanString = TimeSpanString
  { unwrapTimeString :: String
  } deriving (Eq, Ord, Show)

instance Arbitrary TimeSpanString where
  arbitrary = do
    h1 <- elements ['0' .. '2']
    h2 <-
      if h1 == '2'
        then elements ['0' .. '4']
        else elements ['0' .. '9']
    m1 <- elements ['0' .. '5']
    m2 <- elements ['0' .. '9']
    h3 <- elements ['0' .. '2']
    h4 <-
      if h3 == '2'
        then elements ['0' .. '4']
        else elements ['0' .. '9']
    m3 <- elements ['0' .. '5']
    m4 <- elements ['0' .. '9']
    let t1 = [h1, h2, ':', m1, m2]
    let t2 = [h3, h4, ':', m3, m4]
    let tr | t1 == t2 = "00:00-" ++ t1
           | t1 < t2 = t1 ++ "-" ++ t2
           | otherwise = t2 ++ "-" ++ t1
    return $ TimeSpanString tr

newtype WeekDayRnd = WeekDayRnd
  { unwrapWeekDayRnd :: WeekDay
  } deriving (Eq, Ord, Show)

instance Arbitrary WeekDayRnd where
  arbitrary = WeekDayRnd <$> elements [Mon .. Sun]

-- Property tests
timeProps :: TestTree
timeProps = testGroup
  "Availability.hs"
    -- 'dayAvailableFromTo /=' test
  [ testProperty "dayAvailableFromTo /=" $ \t1 t2 ->
    t1 /= t2 ==> dayAvailableFromTo [unwrapTimeString t1] /= dayAvailableFromTo
      [unwrapTimeString t2]

    -- 'dayAvailableFromTo ==' test
  , testProperty "dayAvailableFromTo ==" $ \t ->
    dayAvailableFromTo [unwrapTimeString t]
      == dayAvailableFromTo [unwrapTimeString t]

    -- 'double daily availability' test
  , testProperty "dayAvailableFromTo two timespans" $ \t1 t2 ->
    isJust $ dayAvailableFromTo [unwrapTimeString t1, unwrapTimeString t2]

    -- 'reserve daily availability' test
  , testProperty "reserve all daily availability" $ \t -> do
    let u = unwrapTimeString t
    (dayAvailableFromTo [u] >>= dayReserveTime u) == Just dayNotAvailable

    -- 'weekAvailableFromTo /=' test
  , testProperty "weekAvailableFromTo /=" $ \t1 t2 w1 w2 ->
    w1
      /=  w2
      ==> weekAvailableFromTo [(unwrapWeekDayRnd w1, unwrapTimeString t1)]
      /=  weekAvailableFromTo [(unwrapWeekDayRnd w2, unwrapTimeString t2)]

    -- 'weekAvailableFromTo ==' test
  , testProperty "weekAvailableFromTo ==" $ \t w ->
    weekAvailableFromTo [(unwrapWeekDayRnd w, unwrapTimeString t)]
      == weekAvailableFromTo [(unwrapWeekDayRnd w, unwrapTimeString t)]

    -- 'double weekly availability' test
  , testProperty "weekAvailableFromTo two timespans"
    $ \t1 t2 w1 w2 -> isJust $ weekAvailableFromTo
        [ (unwrapWeekDayRnd w1, unwrapTimeString t1)
        , (unwrapWeekDayRnd w2, unwrapTimeString t2)
        ]

    -- 'reserve weeekyl availability' test
  , testProperty "reserve all weekly availability" $ \t w -> do
    let u = unwrapTimeString t
    let v = unwrapWeekDayRnd w
    (weekAvailableFromTo [(v, u)] >>= weekReserveTime (v, u))
      == Just weekNotAvailable
  ]

-- Availability.hs related tests
-- Unit tests
timeSpec :: Spec
timeSpec = parallel $ do
  it "should succeed to set daily availability"
    $          return dayAvailable
    `shouldBe` dayAvailableFromTo ["0-0"]

  it "should succeed to create daily non availability"
    $          return dayNotAvailable
    `shouldBe` dayNotAvailableFromTo ["00:00-00:00"]

  it "should succeed to compare daily availability"
    $          dayAvailableFromTo ["12:30-13:30"]
    `shouldBe` dayNotAvailableFromTo ["00:00-12:30", "13:30-00:00"]

  it "should fail to reserve time in daily non availability"
    $          dayReserveTime "05:30-06:30" dayNotAvailable
    `shouldBe` Nothing

  it "should succeed to reserve all daily time"
    $ (dayAvailableFromTo ["06:23-07:12"] >>= dayReserveTime "06:23-07:12")
    `shouldBe` Just dayNotAvailable

  it "should succeed to multiple reserve daily time"
    $          (   dayAvailableFromTo ["15:55-17:45"]
               >>= dayReserveTime "15:55-16:25"
               >>= dayReserveTime "16:25-17:45"
               )
    `shouldBe` Just dayNotAvailable

  it "should succeed to reserve daily time with partial Availability"
    $          (   dayAvailableFromTo ["15:55-17:45", "22:22-23:49"]
               >>= dayReserveTime "15:55-17:45"
               >>= dayReserveTime "22:22-23:49"
               )
    `shouldBe` Just dayNotAvailable

  it "should fail to reserve daily time above boundaries"
    $ (dayAvailableFromTo ["10:00-11:00"] >>= dayReserveTime "09:59-11:01")
    `shouldBe` Nothing

  it "should fail to create daily availability with wrong timespan"
    $          dayAvailableFromTo ["12:12-09:09"]
    `shouldBe` Nothing

  it "should fail to create daily availability with wrong time"
    $          dayAvailableFromTo ["09:09"]
    `shouldBe` Nothing

  it "should succeed to encode daily availability as YAML"
    $          encode (dayAvailableFromTo ["02:12-09:10"])
    `shouldBe` pack "- - 02:12:00\n  - 09:09:00\n"

  it "should succeed to encode non daily availability YAML"
    $          encode dayNotAvailable
    `shouldBe` pack "[]\n"

  it "should succeed to encode full daily availability YAML"
    $          encode dayAvailable
    `shouldBe` pack "- - 00:00:00\n  - 23:59:00\n"

  it "should succeed to decode daily availability as YAML"
    $          dayAvailableFromTo ["02:12-09:10"]
    `shouldBe` decode (pack "- - 02:12:00\n  - 09:09:00\n")

  it "should succeed to decode non daily availability YAML"
    $          return dayNotAvailable
    `shouldBe` decode (pack "[]\n")

  it "should succeed to decode full daily availability YAML"
    $          return dayAvailable
    `shouldBe` decode (pack "- - 00:00:00\n  - 23:59:00\n")

  it "should succeed to set weekly availability"
    $          return weekAvailable
    `shouldBe` weekAvailableFromTo
                 [ (Mon, "00:00-00:00")
                 , (Tue, "00:00-00:00")
                 , (Wed, "00:00-00:00")
                 , (Thu, "00:00-00:00")
                 , (Fri, "00:00-00:00")
                 , (Sat, "00:00-00:00")
                 , (Sun, "00:00-00:00")
                 ]

  it "should succeed to create weekly non availability"
    $          return weekNotAvailable
    `shouldBe` weekNotAvailableFromTo
                 [ (Mon, "00:00-00:00")
                 , (Tue, "00:00-00:00")
                 , (Wed, "00:00-12:50")
                 , (Wed, "12:50-00:00")
                 , (Thu, "00:00-00:00")
                 , (Fri, "00:00-00:00")
                 , (Sat, "00:00-00:00")
                 , (Sun, "00:00-00:00")
                 ]

  it "should succeed to create a list from weekly availability"
    $          toList weekAvailable
    `shouldBe` [ "0:00-0:00"
               , "0:00-0:00"
               , "0:00-0:00"
               , "0:00-0:00"
               , "0:00-0:00"
               , "0:00-0:00"
               , "0:00-0:00"
               ]

  it "should succeed to create a list from weekly non availability"
    $          toList weekNotAvailable
    `shouldBe` ["", "", "", "", "", "", ""]

  it "should succeed to create compare weekly availability"
    $          weekAvailableFromTo [(Wed, "11:30-13:30")]
    `shouldBe` weekNotAvailableFromTo
                 [ (Mon, "00:00-00:00")
                 , (Tue, "00:00-00:00")
                 , (Wed, "00:00-11:30")
                 , (Wed, "13:30-00:00")
                 , (Thu, "00:00-00:00")
                 , (Fri, "00:00-00:00")
                 , (Sat, "00:00-00:00")
                 , (Sun, "00:00-00:00")
                 ]

  it "should fail to reserve time in weekly non availability"
    $          weekReserveTime (Mon, "05:30-06:30") weekNotAvailable
    `shouldBe` Nothing

  it "should succeed to reserve all weekly time"
    $          (   weekAvailableFromTo [(Mon, "06:23-07:12")]
               >>= weekReserveTime (Mon, "06:23-07:12")
               )
    `shouldBe` Just weekNotAvailable

  it "should succeed to multiple reserve weekly time"
    $ (   weekAvailableFromTo [(Mon, "06:23-07:12"), (Tue, "09:12-22:22")]
      >>= weekReserveTime (Mon, "06:23-07:12")
      >>= weekReserveTime (Tue, "09:12-22:22")
      )
    `shouldBe` Just weekNotAvailable

  it "should succeed to reserve weekly time with partial Availability"
    $ (   weekAvailableFromTo [(Mon, "15:55-17:45"), (Mon, "22:22-23:49")]
      >>= weekReserveTime (Mon, "15:55-17:45")
      >>= weekReserveTime (Mon, "22:22-23:49")
      )
    `shouldBe` Just weekNotAvailable

  it "should fail to reserve weekly time above boundaries"
    $          (   weekAvailableFromTo [(Mon, "10:00-11:00")]
               >>= weekReserveTime (Mon, "09:59-11:01")
               )
    `shouldBe` Nothing

  it "should fail to create weekly availability with wrong timespan"
    $          weekAvailableFromTo [(Sun, "12:12-09:09")]
    `shouldBe` Nothing

  it "should fail to create weekly availability with wrong time"
    $          weekAvailableFromTo [(Fri, "09:09")]
    `shouldBe` Nothing

  it "should succeed to encode weekly availability as YAML"
    $          encode (weekAvailableFromTo [(Thu, "02:12-09:10")])
    `shouldBe` pack
                 "- - Mon\n\
                        \  - []\n\
                        \- - Tue\n\
                        \  - []\n\
                        \- - Wed\n\
                        \  - []\n\
                        \- - Thu\n\
                        \  - - - 02:12:00\n\
                        \      - 09:09:00\n\
                        \- - Fri\n\
                        \  - []\n\
                        \- - Sat\n\
                        \  - []\n\
                        \- - Sun\n\
                        \  - []\n"
  it "should succeed to encode non weekly availability YAML"
    $          encode weekNotAvailable
    `shouldBe` pack
                 "- - Mon\n\
                        \  - []\n\
                        \- - Tue\n\
                        \  - []\n\
                        \- - Wed\n\
                        \  - []\n\
                        \- - Thu\n\
                        \  - []\n\
                        \- - Fri\n\
                        \  - []\n\
                        \- - Sat\n\
                        \  - []\n\
                        \- - Sun\n\
                        \  - []\n"

  it "should succeed to encode full weekly availability YAML"
    $          encode weekAvailable
    `shouldBe` pack
                 "- - Mon\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Tue\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Wed\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Thu\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Fri\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Sat\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Sun\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n"

  it "should succeed to decode weekly availability as YAML"
    $          weekAvailableFromTo [(Thu, "02:12-09:10")]
    `shouldBe` decode
                 ( pack
                   "- - Mon\n\
                        \  - []\n\
                        \- - Tue\n\
                        \  - []\n\
                        \- - Wed\n\
                        \  - []\n\
                        \- - Thu\n\
                        \  - - - 02:12:00\n\
                        \      - 09:09:00\n\
                        \- - Fri\n\
                        \  - []\n\
                        \- - Sat\n\
                        \  - []\n\
                        \- - Sun\n\
                        \  - []\n"
                 )

  it "should succeed to decode non weekly availability YAML"
    $          return weekNotAvailable
    `shouldBe` decode
                 ( pack
                   "- - Mon\n\
                        \  - []\n\
                        \- - Tue\n\
                        \  - []\n\
                        \- - Wed\n\
                        \  - []\n\
                        \- - Thu\n\
                        \  - []\n\
                        \- - Fri\n\
                        \  - []\n\
                        \- - Sat\n\
                        \  - []\n\
                        \- - Sun\n\
                        \  - []\n"
                 )

  it "should succeed to decode full weekly availability YAML"
    $          return weekAvailable
    `shouldBe` decode
                 ( pack
                   "- - Mon\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Tue\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Wed\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Thu\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Fri\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Sat\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Sun\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n"
                 )

  it "should succeed format time"
    $          dateTimeFormat testTime
    `shouldBe` "01.01.00  0:00"

  it "should succeed convert to local time" $ do
    let result = unTestFixture (utcToLocal testTime) fixture
    result `shouldBe` "01.01.00  0:00"

  it "should succeed convert to local time with day offset" $ do
    let ff =
          def { _getCurrentTimeZone' = return $ minutesToTimeZone (60 * 24) }
    let result = unTestFixture (utcToLocal testTime) ff
    result `shouldBe` "02.01.00  0:00"

  it "should succeed to evaluate end time just fitting"
    $          show
                 ( evaluateEnd (UTCTime (fromGregorian 0 0 0) 0)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 60)
                 )
    `shouldBe` "Just 0000-01-03 02:00:00 UTC"

  it "should succeed to evaluate end time just fitting differnt time"
    $          show
                 ( evaluateEnd (UTCTime (fromGregorian 0 0 0) 0)
                               (fromJust $ weekAvailableFromTo [(Fri, "13:38-13:56")])
                               (Duration 18)
                 )
    `shouldBe` "Just 0000-01-07 13:56:00 UTC"

  it "should succeed to evaluate end time not fitting to next week"
    $          show
                 ( evaluateEnd (UTCTime (fromGregorian 0 0 0) 0)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 70)
                 )
    `shouldBe` "Just 0000-01-10 01:10:00 UTC"

  it "should succeed to evaluate end time not fitting to next day"
    $          show
                 ( evaluateEnd
                   (UTCTime (fromGregorian 0 0 0) 0)
                   (fromJust $ weekAvailableFromTo [(Mon, "1-2"), (Tue, "2-3")])
                   (Duration 70)
                 )
    `shouldBe` "Just 0000-01-04 02:10:00 UTC"

  it "should succeed to evaluate end time not fitting to next year"
    $          show
                 ( evaluateEnd (UTCTime (fromGregorian 2010 12 30) 0)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 70)
                 )
    `shouldBe` "Just 2011-01-10 01:10:00 UTC"

  it "should succeed to evaluate end time not fitting short timespan"
    $          show
                 ( evaluateEnd (UTCTime (fromGregorian 0 0 0) 0)
                               (fromJust $ weekAvailableFromTo [(Fri, "10:10-10:15")])
                               (Duration 222)
                 )
    `shouldBe` "Just 0000-11-10 10:12:00 UTC"

  it "should succeed to evaluate end time just fitting with offset"
    $          show
                 ( evaluateEnd (UTCTime (fromGregorian 0 0 3) 5400)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 30)
                 )
    `shouldBe` "Just 0000-01-03 02:00:00 UTC"

  it "should succeed to evaluate end time not fitting to next day with offset"
    $          show
                 ( evaluateEnd
                   (UTCTime (fromGregorian 0 0 3) 5400)
                   (fromJust $ weekAvailableFromTo [(Mon, "1-2"), (Tue, "4-5")])
                   (Duration 60)
                 )
    `shouldBe` "Just 0000-01-04 04:30:00 UTC"

  it "should succeed to evaluate end time with too large offset"
    $          show
                 ( evaluateEnd
                   (UTCTime (fromGregorian 0 0 3) 86400)
                   (fromJust $ weekAvailableFromTo [(Mon, "0-0"), (Tue, "1-2")])
                   (Duration 10)
                 )
    `shouldBe` "Just 0000-01-04 01:10:00 UTC"

  it "should fail to evaluate end time with no availability"
    $          evaluateEnd (UTCTime (fromGregorian 0 0 0) 0)
                           weekNotAvailable
                           (Duration 0)
    `shouldBe` Nothing

  it "should succeed to evaluate start time"
    $          show
                 ( evaluateStart (UTCTime (fromGregorian 0 0 0) 0)
                                 (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                 )
    `shouldBe` "Just 0000-01-03 01:00:00 UTC"

  it "should succeed to evaluate start time with offset same day"
    $          show
                 ( evaluateStart (UTCTime (fromGregorian 0 0 3) 5400)
                                 (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                 )
    `shouldBe` "Just 0000-01-03 01:30:00 UTC"

  it "should succeed to evaluate start time with offset other day"
    $          show
                 ( evaluateStart (UTCTime (fromGregorian 0 0 0) 5400)
                                 (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                 )
    `shouldBe` "Just 0000-01-03 01:00:00 UTC"

  it "should fail to evaluate start time with no availability"
    $ evaluateStart (UTCTime (fromGregorian 0 0 0) 5400) weekNotAvailable
    `shouldBe` Nothing

  it "should succeed to evaluate daily just fitting"
    $          ( evaluateDaily (UTCTime (fromGregorian 0 0 0) 0)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 60)
               )
    `shouldBe` Just [(fromGregorian 0 0 3, " 1:00", " 2:00")]

  it "should succeed to evaluate daily just fitting different time"
    $ ( evaluateDaily (UTCTime (fromGregorian 0 0 0) 0)
                      (fromJust $ weekAvailableFromTo [(Fri, "18:32-18:45")])
                      (Duration 13)
      )
    `shouldBe` Just [(fromGregorian 0 0 7, "18:32", "18:45")]

  it "should succeed to evaluate end time not fitting to next week"
    $          ( evaluateDaily (UTCTime (fromGregorian 0 0 0) 0)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 70)
               )
    `shouldBe` Just
                 [ (fromGregorian 0 0 3 , " 1:00", " 2:00")
                 , (fromGregorian 0 0 10, " 1:00", " 1:10")
                 ]

  it "should succeed to evaluate end time with offset"
    $          ( evaluateDaily (UTCTime (fromGregorian 2018 0 8) 5400)
                               (fromJust $ weekAvailableFromTo [(Mon, "1-2")])
                               (Duration 30)
               )
    `shouldBe` Just [(fromGregorian 2018 0 8, " 1:30", " 2:00")]

  it "should fail to evaluate daily if not available"
    $          ( evaluateDaily (UTCTime (fromGregorian 0 0 0) 0)
                               weekNotAvailable
                               (Duration 60)
               )
    `shouldBe` Nothing
