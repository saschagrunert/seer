-- | The Action tests
--
-- @since 0.1.0

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ActionSpec
  ( actionProps
  , actionSpec
  ) where

import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.Maybe                   (isJust
                                    ,isNothing)
import Seer.Action                  (ActionSpec (ActionSpec)
                                    ,MonadAction
                                    ,new)
import Seer.Manifest                (toList)
import Test.Tasty                   (TestTree
                                    ,testGroup)
import Test.Tasty.QuickCheck        (testProperty)
import Seer.Time                    (Duration (Duration))
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import TestData                     (testMetadata)

mkFixture "Fixture" [ts| MonadAction |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _newMetadata' = return testMetadata }

testActionData :: String -> String -> Int -> ActionSpec
testActionData a b c = ActionSpec a (Just b) (Duration c)

-- Action.hs related tests
-- Unit tests
actionSpec :: Spec
actionSpec = parallel $ do
  it "should succeed to create a new Action without description" $ do
    let result = unTestFixture (new "n1" Nothing "2h") fixture
    isJust result `shouldBe` True

  it "should succeed to create a new Action with description" $ do
    let result = unTestFixture (new "n1" (Just "description") "2h") fixture
    isJust result `shouldBe` True

  it "should fail to create a new Action if duration parsing fails" $ do
    let result = unTestFixture (new "n1" Nothing "test") fixture
    isNothing result `shouldBe` True

-- Property tests
actionProps :: TestTree
actionProps = testGroup
  "ActionSpec.hs"
  [ testProperty "toList"
      $ \a b c -> toList (testActionData a b c) == [a, b, show $ Duration c]
  ]
