module ScheduleSpec
  ( scheduleSpec
  ) where

import Data.Time.Clock  (getCurrentTime)
import Data.UUID.V4     (nextRandom)
import Seer.Schedule    (new)
import Test.Tasty.Hspec (Spec, it, parallel, shouldNotBe)

-- Schedule.hs related tests
-- Unit tests
scheduleSpec :: Spec
scheduleSpec = parallel $ it "should succeed to compare two new Schedules" $ do
  t  <- getCurrentTime
  u  <- nextRandom
  m1 <- new t t u u
  m2 <- new t t u u
  m1 `shouldNotBe` m2
