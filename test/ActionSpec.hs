module ActionSpec
  ( actionProps
  , actionSpec
  ) where

import Seer.Action           (ActionSpec (ActionSpec), Duration (Duration), new)
import Seer.Manifest         (toList)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Hspec      (Spec, it, parallel, shouldNotBe)
import Test.Tasty.QuickCheck (testProperty)

testActionData :: String -> String -> Int -> ActionSpec
testActionData a b c = ActionSpec a (Just b) (Duration c)

-- Action.hs related tests
-- Unit tests
actionSpec :: Spec
actionSpec = parallel $ it "should succeed to compare two new Actions" $ do
  m1 <- new "n1" Nothing "2h"
  m2 <- new "n1" Nothing "2h"
  m1 `shouldNotBe` m2

-- Property tests
actionProps :: TestTree
actionProps = testGroup
  "ActionSpec.hs"
  [ testProperty "toList"
      $ \a b c -> toList (testActionData a b c) == [a, b, show c ++ "m"]
  ]
