module ActionSpec
  ( actionSpec
  ) where

import Seer.Action      (new)
import Test.Tasty.Hspec (Spec, it, parallel, shouldNotBe)

-- Action.hs related tests
-- Unit tests
actionSpec :: Spec
actionSpec = parallel $ it "should succeed to compare two new Actions" $ do
  m1 <- new "n1" Nothing "2h"
  m2 <- new "n1" Nothing "2h"
  m1 `shouldNotBe` m2
