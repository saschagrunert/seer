-- | The Manifest tests
--
-- @since 0.1.0

module ManifestSpec
  ( manifestSpec
  ) where

import Control.Lens                 ((^.))
import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.UUID                    (nil)
import Seer.Manifest                (MonadManifest
                                    ,creationTimestamp
                                    ,currentMetadata
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
  let result = unTestFixture currentMetadata fixture
  result ^. creationTimestamp `shouldBe` testTime
  result ^. uid `shouldBe` nil
