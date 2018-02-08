{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module SeerSpec
  ( seerSpec
  ) where

import Control.Monad.TestFixture    (unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.Either                  (isLeft)
import Seer                         (MonadSeer, getStorages)
import System.IO.Error              (userError)
import Test.Tasty.Hspec             (Spec, it, parallel, shouldBe)

mkFixture "Fixture" [ts| MonadSeer |]

-- Seer.hs related tests
-- Unit tests
seerSpec :: Spec
seerSpec = parallel $ do
  it "should succeed to get Storages" $ do
    let ff = def {_list' = return $ Right [["name 1", "remote 1"]
                                          ,["name 2", "remote 2"]]}
    let result = unTestFixture getStorages ff
    result `shouldBe` Right "NAME    REMOTE  \n\
                            \name 1  remote 1\n\
                            \name 2  remote 2\n"

  it "should fail to get Storages if listing fails" $ do
    let ff = def {_list' = return . Left $ userError "" }
    let result = unTestFixture getStorages ff
    isLeft result `shouldBe` True
