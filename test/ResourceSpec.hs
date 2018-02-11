module ResourceSpec
  ( resourceProps
  , resourceSpec
  ) where

import Seer.Availability     (weekAvailable, weekNotAvailable)
import Seer.Manifest         (toList)
import Seer.Resource         (ResourceSpec (ResourceSpec), new)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.Hspec      (Spec, it, parallel, shouldNotBe)
import Test.Tasty.QuickCheck (testProperty)

testResourceData :: String -> String -> ResourceSpec
testResourceData a b = ResourceSpec a (Just b) weekNotAvailable

-- Resource.hs related tests
-- Unit tests
resourceSpec :: Spec
resourceSpec = parallel $ it "should succeed to compare two new Resources" $ do
  m1 <- new "n1" Nothing weekAvailable
  m2 <- new "n1" Nothing weekAvailable
  m1 `shouldNotBe` m2

-- Property tests
resourceProps :: TestTree
resourceProps = testGroup
  "ResourceSpec.hs"
  [ testProperty "toList" $ \a b ->
      toList (testResourceData a b) == [a, b, "", "", "", "", "", "", ""]
  ]
