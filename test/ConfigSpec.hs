module ConfigSpec
  ( configSpec
  ) where

import Seer.Config      (new)
import Test.Tasty.Hspec (Spec, it, parallel, shouldNotBe)

-- Config.hs related tests
-- Unit tests
configSpec :: Spec
configSpec = parallel $ it "should succeed to compare two new Configs" $ do
  m1 <- new
  m2 <- new
  m1 `shouldNotBe` m2
