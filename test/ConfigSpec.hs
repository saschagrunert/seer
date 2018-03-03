-- | The Config tests
--
-- @since 0.1.0

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ConfigSpec
  ( configProps
  , configSpec
  ) where

import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Seer.Config                  (ConfigSpec (ConfigSpec)
                                    ,MonadConfig
                                    ,new
                                    ,storage
                                    ,toList)
import Seer.Manifest                (spec)
import Test.Tasty                   (TestTree
                                    ,testGroup)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import Test.Tasty.QuickCheck        (testProperty)
import TestData                     (testMetadata)

mkFixture "Fixture" [ts| MonadConfig |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _newMetadata' = return testMetadata }

-- Config.hs related tests
-- Unit tests
configSpec :: Spec
configSpec = parallel $ it "should succeed to create a new Config" $ do
  let result = unTestFixture (new "config") fixture
  storage (spec result) `shouldBe` "config"

-- Property tests
configProps :: TestTree
configProps = testGroup
  "ConfigSpec.hs"
  [testProperty "toList" $ \a -> toList (ConfigSpec a) == [a]]
