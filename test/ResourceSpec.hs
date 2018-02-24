{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ResourceSpec
  ( resourceProps
  , resourceSpec
  ) where

import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Seer.Manifest                (spec
                                    ,toList)
import Seer.Resource                (MonadResource
                                    ,ResourceSpec (ResourceSpec
                                                  ,description)
                                    ,new)
import Test.Tasty                   (TestTree
                                    ,testGroup)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import Test.Tasty.QuickCheck        (testProperty)
import Seer.Time                    (weekAvailable
                                    ,weekNotAvailable)
import TestData                     (testMetadata)

mkFixture "Fixture" [ts| MonadResource |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _newMetadata' = return testMetadata }

testResourceData :: String -> String -> ResourceSpec
testResourceData a b = ResourceSpec a (Just b) weekNotAvailable

-- Resource.hs related tests
-- Unit tests
resourceSpec :: Spec
resourceSpec = parallel $ do
  it "should succeed to create a new Resource without description" $ do
    let result = unTestFixture (new "n1" Nothing weekAvailable) fixture
    description (spec result) `shouldBe` Nothing

  it "should succeed to create a new Resource with description" $ do
    let result =
          unTestFixture (new "n1" (Just "description") weekAvailable) fixture
    description (spec result) `shouldBe` Just "description"

-- Property tests
resourceProps :: TestTree
resourceProps = testGroup
  "ResourceSpec.hs"
  [ testProperty "toList" $ \a b ->
      toList (testResourceData a b) == [a, b, "", "", "", "", "", "", ""]
  ]
