-- | The View tests
--
-- @since 0.1.0

module ViewSpec
  ( viewSpec
  ) where

import Data.Time.Calendar    (fromGregorian)
import Data.Time.Clock       (UTCTime(UTCTime))
import Seer.View             (createView)
import Test.Tasty.Hspec      (Spec
                             ,it
                             ,parallel
                             ,shouldBe)
import TestData              (testAction
                             ,testResource
                             ,testSchedule
                             ,testTime)

-- View.hs related tests
-- Unit tests
viewSpec :: Spec
viewSpec = parallel $ do
  it "should succeed to create a new View"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          Nothing
                          Nothing
    `shouldBe` Just
                 "Sat [01.01]  0:00 ↦ action (resource)\n\
                 \             0:05 ⇥ action (resource)"

  it "should succeed to create a new View with filtered Resource"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          (Just "resource")
                          Nothing
    `shouldBe` Just
                 "Sat [01.01]  0:00 ↦ action\n\
                 \             0:05 ⇥ action"

  it "should succeed to create a new View with filtered Action"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          Nothing
                          (Just "action")
    `shouldBe` Just
                 "Sat [01.01]  0:00 ↦ resource\n\
                 \             0:05 ⇥ resource"

  it "should succeed to create a new View with filtered Action and Resource"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          (Just "resource")
                          (Just "action")
    `shouldBe` Just
                 "Sat [01.01]  0:00 ↦\n\
                 \             0:05 ⇥"

  it "should fail to create a new View with wrong filtered Resource"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          (Just "not existing")
                          Nothing
    `shouldBe` Just ""

  it "should fail to create a new View with wrong filtered Action"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          Nothing
                          (Just "not existing")
    `shouldBe` Just ""

  it "should fail to create a new View with no Schedules"
    $          createView []
                          [testResource]
                          [testAction]
                          testTime
                          testTime
                          Nothing
                          Nothing
    `shouldBe` Just ""

  it "should fail to create a new View with no Resources"
    $          createView [testSchedule]
                          []
                          [testAction]
                          testTime
                          testTime
                          Nothing
                          Nothing
    `shouldBe` Nothing

  it "should fail to create a new View with no Action"
    $          createView [testSchedule]
                          [testResource]
                          []
                          testTime
                          testTime
                          Nothing
                          Nothing
    `shouldBe` Nothing

  it "should fail to create a new View with no Entity"
    $          createView [] [] [] testTime testTime Nothing Nothing
    `shouldBe` Just ""

  it "should fail to create a new View wrong time"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          (UTCTime (fromGregorian 10 0 0) 0)
                          testTime
                          Nothing
                          Nothing
    `shouldBe` Just ""

  it "should fail to create a new View with future time"
    $          createView [testSchedule]
                          [testResource]
                          [testAction]
                          (UTCTime (fromGregorian 10 0 0) 0)
                          (UTCTime (fromGregorian 20 0 0) 0)
                          Nothing
                          Nothing
    `shouldBe` Just ""
