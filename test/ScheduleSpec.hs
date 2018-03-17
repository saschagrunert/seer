-- | The Schedule tests
--
-- @since 0.1.0

module ScheduleSpec
  ( scheduleSpec
  ) where

import Control.Lens                 ((^.))
import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.UUID                    (nil)
import Seer.Manifest                (spec)
import Seer.Schedule                (MonadSchedule
                                    ,actionID
                                    ,resourceID
                                    ,start
                                    ,new)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import TestData                     (testTime
                                    ,testMetadata)

mkFixture "Fixture" [ts| MonadSchedule |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _currentMetadata' = return (testMetadata 1) }

-- Schedule.hs related tests
-- Unit tests
scheduleSpec :: Spec
scheduleSpec = parallel $ it "should succeed to create a new Schedule" $ do
  let result = unTestFixture (new testTime nil nil) fixture
  result ^. spec . start `shouldBe` testTime
  result ^. spec . resourceID `shouldBe` nil
  result ^. spec . actionID `shouldBe` nil
