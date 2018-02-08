module ResourceSpec
  ( resourceSpec
  ) where

import Seer.Availability (weekAvailable)
import Seer.Resource     (new)
import Test.Tasty.Hspec  (Spec, it, parallel, shouldNotBe)

-- Resource.hs related tests
-- Unit tests
resourceSpec :: Spec
resourceSpec = parallel $ it "should succeed to compare two new Resources" $ do
  m1 <- new "n1" Nothing weekAvailable
  m2 <- new "n1" Nothing weekAvailable
  m1 `shouldNotBe` m2
