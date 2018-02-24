module UtilsSpec
  ( utilsProps
  , utilsSpec
  ) where

import Seer.Utils                   (rstrip)
import Test.Tasty                   (TestTree
                                    ,testGroup)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe
                                    ,shouldNotBe)
import Test.Tasty.QuickCheck        (testProperty)

-- Utils.hs related tests
-- Unit tests
utilsSpec :: Spec
utilsSpec = parallel $ do
  it "should succeed to right strip a string" $ rstrip "abc " `shouldBe` "abc"

  it "should fail to left strip a string" $ rstrip " abc" `shouldNotBe` "abc"

-- Property tests
utilsProps :: TestTree
utilsProps = testGroup
  "UtilsSpec.hs"
  [testProperty "rstrip" $ \a x -> rstrip (a ++ replicate x ' ') == rstrip a]
