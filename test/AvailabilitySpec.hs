module AvailabilitySpec
  ( availabilityProps
  , availabilitySpec
  ) where

import Data.ByteString.Char8 (pack)
import Data.Maybe            (isJust)
import Data.Yaml             (decode, encode)
import Seer.Availability     (WeekDay (..), dayAvailable, dayAvailableFromTo,
                              dayNotAvailable, dayNotAvailableFromTo,
                              dayReserveTime, weekAvailable,
                              weekAvailableFromTo, weekNotAvailable,
                              weekNotAvailableFromTo, weekReserveTime)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Hspec      (Spec, it, parallel, shouldBe)
import Test.Tasty.QuickCheck (Arbitrary (..), elements, testProperty, (==>))

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
    let tr =
          if t1 <= t2
            then t1 ++ "-" ++ t2
            else t2 ++ "-" ++ t1
    return $ TimeSpanString tr

newtype WeekDayRnd = WeekDayRnd
  { unwrapWeekDayRnd :: WeekDay
  } deriving (Eq, Ord, Show)

instance Arbitrary WeekDayRnd where
  arbitrary = WeekDayRnd <$> elements [Monday .. Sunday]

-- Property tests
availabilityProps :: TestTree
availabilityProps = testGroup
  "Availability.hs"
      -- 'dayAvailableFromTo /=' test
  [ testProperty "dayAvailableFromTo /="
    $ \t1 t2 -> t1 /= t2 ==> dayAvailableFromTo [unwrapTimeString t1] /= dayAvailableFromTo [unwrapTimeString t2]
      -- 'dayAvailableFromTo ==' test
  , testProperty "dayAvailableFromTo =="
    $ \t -> dayAvailableFromTo [unwrapTimeString t] == dayAvailableFromTo [unwrapTimeString t]
      -- 'double daily availability' test
  , testProperty "dayAvailableFromTo two timespans"
    $ \t1 t2 -> isJust $ dayAvailableFromTo [unwrapTimeString t1, unwrapTimeString t2]
      -- 'reserve daily availability' test
  , testProperty "reserve all daily availability" $ \t -> do
    let u = unwrapTimeString t
    (dayAvailableFromTo [u] >>= dayReserveTime u) == Just dayNotAvailable
      -- 'weekAvailableFromTo /=' test
  , testProperty "weekAvailableFromTo /=" $ \t1 t2 w1 w2 ->
    w1 /= w2 ==> weekAvailableFromTo [(unwrapWeekDayRnd w1, unwrapTimeString t1)] /= weekAvailableFromTo
      [(unwrapWeekDayRnd w2, unwrapTimeString t2)]
      -- 'weekAvailableFromTo ==' test
  , testProperty "weekAvailableFromTo ==" $ \t w -> weekAvailableFromTo [(unwrapWeekDayRnd w, unwrapTimeString t)]
    == weekAvailableFromTo [(unwrapWeekDayRnd w, unwrapTimeString t)]
      -- 'double weekly availability' test
  , testProperty "weekAvailableFromTo two timespans" $ \t1 t2 w1 w2 -> isJust
    $ weekAvailableFromTo [(unwrapWeekDayRnd w1, unwrapTimeString t1), (unwrapWeekDayRnd w2, unwrapTimeString t2)]
      -- 'reserve weeekyl availability' test
  , testProperty "reserve all weekly availability" $ \t w -> do
    let u = unwrapTimeString t
    let v = unwrapWeekDayRnd w
    (weekAvailableFromTo [(v, u)] >>= weekReserveTime (v, u)) == Just weekNotAvailable
  ]

-- Availability.hs related tests
-- Unit tests
availabilitySpec :: Spec
availabilitySpec = parallel $ do
  it "should succeed to set daily availability" $ return dayAvailable `shouldBe` dayAvailableFromTo ["00:00-23:59"]
  it "should succeed to create daily non availability" $ return dayNotAvailable `shouldBe` dayNotAvailableFromTo
    ["00:00-23:59"]
  it "should succeed to compare daily availability"
    $          dayAvailableFromTo ["12:30-13:30"]
    `shouldBe` dayNotAvailableFromTo ["00:00-12:29", "13:31-23:59"]
  it "should fail to reserve time in daily non availability"
    $          dayReserveTime "05:30-06:30" dayNotAvailable
    `shouldBe` Nothing
  it "should succeed to reserve all daily time"
    $          (dayAvailableFromTo ["06:23-07:12"] >>= dayReserveTime "06:23-07:12")
    `shouldBe` Just dayNotAvailable
  it "should succeed to multiple reserve daily time"
    $          (dayAvailableFromTo ["15:55-17:45"] >>= dayReserveTime "15:55-16:23" >>= dayReserveTime "16:24-17:45")
    `shouldBe` Just dayNotAvailable
  it "should succeed to reserve daily time with partial Availability"
    $          ( dayAvailableFromTo ["15:55-17:45", "22:22-23:49"] >>= dayReserveTime "15:55-17:45" >>= dayReserveTime
                 "22:22-23:49"
               )
    `shouldBe` Just dayNotAvailable
  it "should fail to reserve daily time above boundaries"
    $          (dayAvailableFromTo ["10:00-11:00"] >>= dayReserveTime "09:59-11:01")
    `shouldBe` Nothing
  it "should fail to create daily availability with wrong timespan"
    $          dayAvailableFromTo ["12:12-09:09"]
    `shouldBe` Nothing
  it "should fail to create daily availability with wrong time" $ dayAvailableFromTo ["2:12-09:09"] `shouldBe` Nothing
  it "should succeed to encode daily availability as YAML" $ encode (dayAvailableFromTo ["02:12-09:09"]) `shouldBe` pack
    "- - 02:12:00\n  - 09:09:00\n"
  it "should succeed to encode non daily availability YAML" $ encode dayNotAvailable `shouldBe` pack "[]\n"
  it "should succeed to encode full daily availability YAML" $ encode dayAvailable `shouldBe` pack
    "- - 00:00:00\n  - 23:59:00\n"
  it "should succeed to decode daily availability as YAML" $ dayAvailableFromTo ["02:12-09:09"] `shouldBe` decode
    (pack "- - 02:12:00\n  - 09:09:00\n")
  it "should succeed to decode non daily availability YAML" $ return dayNotAvailable `shouldBe` decode (pack "[]\n")
  it "should succeed to decode full daily availability YAML" $ return dayAvailable `shouldBe` decode
    (pack "- - 00:00:00\n  - 23:59:00\n")
  it "should succeed to set weekly availability" $ return weekAvailable `shouldBe` weekAvailableFromTo
    [ (Monday   , "00:00-23:59")
    , (Tuesday  , "00:00-23:59")
    , (Wednesday, "00:00-23:59")
    , (Thursday , "00:00-23:59")
    , (Friday   , "00:00-23:59")
    , (Saturday , "00:00-23:59")
    , (Sunday   , "00:00-23:59")
    ]
  it "should succeed to create weekly non availability" $ return weekNotAvailable `shouldBe` weekNotAvailableFromTo
    [ (Monday   , "00:00-23:59")
    , (Tuesday  , "00:00-23:59")
    , (Wednesday, "00:00-12:50")
    , (Wednesday, "12:51-23:59")
    , (Thursday , "00:00-23:59")
    , (Friday   , "00:00-23:59")
    , (Saturday , "00:00-23:59")
    , (Sunday   , "00:00-23:59")
    ]
  it "should succeed to create compare weekly availability"
    $          weekAvailableFromTo [(Wednesday, "11:30-13:30")]
    `shouldBe` weekNotAvailableFromTo
                 [ (Monday   , "00:00-23:59")
                 , (Tuesday  , "00:00-23:59")
                 , (Wednesday, "00:00-11:29")
                 , (Wednesday, "13:31-23:59")
                 , (Thursday , "00:00-23:59")
                 , (Friday   , "00:00-23:59")
                 , (Saturday , "00:00-23:59")
                 , (Sunday   , "00:00-23:59")
                 ]
  it "should fail to reserve time in weekly non availability"
    $          weekReserveTime (Monday, "05:30-06:30") weekNotAvailable
    `shouldBe` Nothing
  it "should succeed to reserve all weekly time"
    $          (weekAvailableFromTo [(Monday, "06:23-07:12")] >>= weekReserveTime (Monday, "06:23-07:12"))
    `shouldBe` Just weekNotAvailable
  it "should succeed to multiple reserve weekly time"
    $          (   weekAvailableFromTo [(Monday, "06:23-07:12"), (Tuesday, "09:12-22:22")]
               >>= weekReserveTime (Monday, "06:23-07:12")
               >>= weekReserveTime (Tuesday, "09:12-22:22")
               )
    `shouldBe` Just weekNotAvailable
  it "should succeed to reserve weekly time with partial Availability"
    $          (   weekAvailableFromTo [(Monday, "15:55-17:45"), (Monday, "22:22-23:49")]
               >>= weekReserveTime (Monday, "15:55-17:45")
               >>= weekReserveTime (Monday, "22:22-23:49")
               )
    `shouldBe` Just weekNotAvailable
  it "should fail to reserve weekly time above boundaries"
    $          (weekAvailableFromTo [(Monday, "10:00-11:00")] >>= weekReserveTime (Monday, "09:59-11:01"))
    `shouldBe` Nothing
  it "should fail to create weekly availability with wrong timespan"
    $          weekAvailableFromTo [(Sunday, "12:12-09:09")]
    `shouldBe` Nothing
  it "should fail to create weekly availability with wrong time"
    $          weekAvailableFromTo [(Friday, "2:12-09:09")]
    `shouldBe` Nothing
  it "should succeed to encode weekly availability as YAML"
    $          encode (weekAvailableFromTo [(Thursday, "02:12-09:09")])
    `shouldBe` pack
                 "- - Monday\n\
                        \  - []\n\
                        \- - Tuesday\n\
                        \  - []\n\
                        \- - Wednesday\n\
                        \  - []\n\
                        \- - Thursday\n\
                        \  - - - 02:12:00\n\
                        \      - 09:09:00\n\
                        \- - Friday\n\
                        \  - []\n\
                        \- - Saturday\n\
                        \  - []\n\
                        \- - Sunday\n\
                        \  - []\n"
  it "should succeed to encode non weekly availability YAML"
    $          encode weekNotAvailable
    `shouldBe` pack
                 "- - Monday\n\
                        \  - []\n\
                        \- - Tuesday\n\
                        \  - []\n\
                        \- - Wednesday\n\
                        \  - []\n\
                        \- - Thursday\n\
                        \  - []\n\
                        \- - Friday\n\
                        \  - []\n\
                        \- - Saturday\n\
                        \  - []\n\
                        \- - Sunday\n\
                        \  - []\n"
  it "should succeed to encode full weekly availability YAML"
    $          encode weekAvailable
    `shouldBe` pack
                 "- - Monday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Tuesday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Wednesday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Thursday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Friday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Saturday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Sunday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n"
  it "should succeed to decode weekly availability as YAML"
    $          weekAvailableFromTo [(Thursday, "02:12-09:09")]
    `shouldBe` decode
                 ( pack
                   "- - Monday\n\
                        \  - []\n\
                        \- - Tuesday\n\
                        \  - []\n\
                        \- - Wednesday\n\
                        \  - []\n\
                        \- - Thursday\n\
                        \  - - - 02:12:00\n\
                        \      - 09:09:00\n\
                        \- - Friday\n\
                        \  - []\n\
                        \- - Saturday\n\
                        \  - []\n\
                        \- - Sunday\n\
                        \  - []\n"
                 )
  it "should succeed to decode non weekly availability YAML" $ return weekNotAvailable `shouldBe` decode
    ( pack
      "- - Monday\n\
                        \  - []\n\
                        \- - Tuesday\n\
                        \  - []\n\
                        \- - Wednesday\n\
                        \  - []\n\
                        \- - Thursday\n\
                        \  - []\n\
                        \- - Friday\n\
                        \  - []\n\
                        \- - Saturday\n\
                        \  - []\n\
                        \- - Sunday\n\
                        \  - []\n"
    )
  it "should succeed to decode full weekly availability YAML" $ return weekAvailable `shouldBe` decode
    ( pack
      "- - Monday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Tuesday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Wednesday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Thursday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Friday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Saturday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n\
                        \- - Sunday\n\
                        \  - - - 00:00:00\n\
                        \      - 23:59:00\n"
    )
