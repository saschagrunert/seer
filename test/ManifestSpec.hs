module ManifestSpec
  ( manifestSpec
  ) where

import Seer.Manifest    (newMetadata)
import Test.Tasty.Hspec (Spec, it, parallel, shouldNotBe)

-- Manifest.hs related tests
-- Unit tests
manifestSpec :: Spec
manifestSpec = parallel $ it "should succeed to create new unique Metadata" $ do
  m1 <- newMetadata
  m2 <- newMetadata
  m1 `shouldNotBe` m2
