module ScheduleSpec
  ( scheduleProps
  , scheduleSpec
  ) where

import Data.Time.Calendar    (fromGregorian)
import Data.Time.Clock       (UTCTime (UTCTime, utctDay, utctDayTime), getCurrentTime)
import Data.UUID             (nil)
import Data.UUID.V4          (nextRandom)
import Seer.Manifest         (toList)
import Seer.Schedule         (ScheduleSpec (ScheduleSpec), defaultTimeFormat, new)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Hspec      (Spec, it, parallel, shouldNotBe)
import Test.Tasty.QuickCheck (testProperty)

testScheduleData
  :: Integer -> Int -> Int -> Integer -> Int -> Int -> ScheduleSpec
testScheduleData a b c d e f =
  ScheduleSpec (testTime a b c) (testTime d e f) nil nil

testTime :: Integer -> Int -> Int -> UTCTime
testTime a b c = UTCTime {utctDay = fromGregorian a b c, utctDayTime = 0}

-- Schedule.hs related tests
-- Unit tests
scheduleSpec :: Spec
scheduleSpec = parallel $ it "should succeed to compare two new Schedules" $ do
  t  <- getCurrentTime
  u  <- nextRandom
  m1 <- new t t u u
  m2 <- new t t u u
  m1 `shouldNotBe` m2

-- Property tests
scheduleProps :: TestTree
scheduleProps = testGroup
  "ScheduleSpec.hs"
  [ testProperty "toList" $ \a b c d e f ->
      toList (testScheduleData a b c d e f)
        == [ defaultTimeFormat (testTime a b c)
           , defaultTimeFormat (testTime d e f)
           , ""
           , ""
           ]
  ]
