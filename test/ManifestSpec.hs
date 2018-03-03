-- | The Manifest tests
--
-- @since 0.1.0

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ManifestSpec
  ( manifestSpec
  ) where

import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.UUID                    (nil)
import Seer.Manifest                (MonadManifest
                                    ,creationTimestamp
                                    ,newMetadata
                                    ,uid)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import TestData                     (testTime)

mkFixture "Fixture" [ts| MonadManifest |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _getCurrentTime' = return testTime, _nextRandom' = return nil }

-- Manifest.hs related tests
-- Unit tests
manifestSpec :: Spec
manifestSpec = parallel $ it "should succeed to create a new Metadata" $ do
  let result = unTestFixture newMetadata fixture
  creationTimestamp result `shouldBe` testTime
  uid result `shouldBe` nil
