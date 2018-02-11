module ConfigSpec
  ( configProps
  , configSpec
  ) where

import Seer.Config           (ConfigSpec (ConfigSpec), new, toList)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Hspec      (Spec, it, parallel, shouldNotBe)
import Test.Tasty.QuickCheck (testProperty)

-- Config.hs related tests
-- Unit tests
configSpec :: Spec
configSpec = parallel $ it "should succeed to compare two new Configs" $ do
  m1 <- new "a"
  m2 <- new "b"
  m1 `shouldNotBe` m2

-- Property tests
configProps :: TestTree
configProps = testGroup
  "ConfigSpec.hs"
  [testProperty "toList" $ \a -> toList (ConfigSpec a) == [a]]
