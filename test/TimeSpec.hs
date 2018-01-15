module TimeSpec (
    timeProps,
    timeSpec,
) where

import Data.Maybe (isJust, isNothing)
import Seer.Time (fullAvailable, newTime, newTimeSpan, notAvailable, reserve)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)

-- Availability.hs related tests
-- Unit tests
timeSpec :: Spec
timeSpec = parallel $ do
    it "should succeed to create new valid TimeSpans"
        $          show (newTimeSpan "00:00:00" "23:44:00")
        `shouldBe` "Just (TimeSpan {from = 00:00:00, to = 23:44:00})"

    it "should fail to create impossible TimeSpan"
        $          newTimeSpan "23:00:00" "12:00:00"
        `shouldBe` Nothing

    it "should fail to create impossible TimeSpans (left)"
        $          newTimeSpan "aa-00:00" "23:30:00"
        `shouldBe` Nothing

    it "should fail to create impossible TimeSpans (right)"
        $          newTimeSpan "23:00:00" "23-30:00"
        `shouldBe` Nothing

    it "should succeed to create full Availabilitys"
        $ show [fullAvailable]
        `shouldBe` "[Availability (fromList [TimeSpan {from = 00:00:00, to = 23:59:00}])]"

    it "should succeed to reserve valid TimeSpan"
        $ case newTimeSpan "13:37:00" "14:01:00" of
              Just s ->
                  show (reserve fullAvailable s)
                      `shouldBe` "Just (Availability (fromList [TimeSpan {from = 00:00:00, to = 13:36:00},TimeSpan {from = 14:02:00, to = 23:59:00}]))"
              Nothing -> fail "TimeSpan creation should work"

    it "should fail to reserve already reserved TimeSpan"
        $ case newTimeSpan "13:37:00" "14:01:00" of
              Just s -> case reserve fullAvailable s of
                  Nothing -> fail "First reservation should work"
                  Just k  -> isNothing (reserve k s) `shouldBe` True
              Nothing -> fail "TimeSpan creation should work"

    it "should fail to reserve on not not existing Availability"
        $ case newTimeSpan "20:01:00" "20:02:00" of
              Just s  -> isNothing (reserve notAvailable s) `shouldBe` True
              Nothing -> fail "TimeSpan creation should work"

-- Property tests
timeProps :: TestTree
timeProps = testGroup
    "TimeSpec.hs"
    [
    -- Time read
      testProperty "time creation" $ \t1 t2 ->
          t1 > 0 && t1 < 24 && t2 > 0 && t2 < 59 ==> isJust $ newTime t1 t2
    ]
