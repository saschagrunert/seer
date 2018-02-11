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

import Control.Monad.TestFixture    (TestFixture, unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.Either                  (isLeft, isRight)
import Seer                         (MonadSeer, getActions, getConfig,
                                     getResources, getSchedules, getStorages,
                                     setDefaultStorage)
import TestData                     (testAction, testConfig, testError,
                                     testSchedule, testResource)
import Test.Tasty.Hspec             (Spec, it, parallel, shouldBe)

mkFixture "Fixture" [ts| MonadSeer |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def
  { _list' = return $ Right [["name 1", "remote 1"], ["name 2", "remote 2"]]
  , _loadActions'   = const . return . Right $ replicate 3 testAction
  , _loadConfig'    = return $ Right testConfig
  , _loadResources' = const . return . Right $ replicate 3 testResource
  , _loadSchedules' = const . return . Right $ replicate 3 testSchedule
  , _newConfig'     = const $ return testConfig
  , _saveConfig'    = const . return $ Right ()
  , _storageExist'  = const . return $ Right True
  }

-- Seer.hs related tests
-- Unit tests
seerSpec :: Spec
seerSpec = parallel $ do
  it "should succeed to get Storages" $ do
    let result = unTestFixture getStorages fixture
    result
      `shouldBe` Right
                   "NAME    REMOTE  \n\
                   \name 1  remote 1\n\
                   \name 2  remote 2\n"

  it "should fail to get Storages if listing fails" $ do
    let ff     = def { _list' = return testError }
    let result = unTestFixture getStorages ff
    isLeft result `shouldBe` True

  it "should succeed to set the default Storage" $ do
    let result = unTestFixture (setDefaultStorage "") fixture
    isRight result `shouldBe` True

  it "should fail to set the default Storage if not existing" $ do
    let ff     = def { _storageExist' = const . return $ Right False }
    let result = unTestFixture (setDefaultStorage "") ff
    isLeft result `shouldBe` True

  it "should fail to set the default Storage if Config save failed" $ do
    let ff = def { _storageExist' = const . return $ Right True
                 , _newConfig'    = const $ return testConfig
                 , _saveConfig'   = const $ return testError
                 }
    let result = unTestFixture (setDefaultStorage "") ff
    isLeft result `shouldBe` True

  it "should fail to set the default Storage if Storage retrieval failed" $ do
    let ff     = def { _storageExist' = const $ return testError }
    let result = unTestFixture (setDefaultStorage "") ff
    isLeft result `shouldBe` True

  it "should succeed to get the Config" $ do
    let result = unTestFixture getConfig fixture
    result `shouldBe` Right "DEFAULT STORAGE\nname           \n"

  it "should fail to get the Config if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getConfig ff
    isLeft result `shouldBe` True

  it "should succeed to get Actions" $ do
    let result = unTestFixture getActions fixture
    result
      `shouldBe` Right
                   "NAME  DESCRIPTION  DURATION\n\
                   \name               0m      \n\
                   \name               0m      \n\
                   \name               0m      \n"

  it "should fail to get Actions if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getActions ff
    isLeft result `shouldBe` True

  it "should fail to get Actions if Action load fails" $ do
    let ff = def { _loadConfig'  = return $ Right testConfig
                 , _loadActions' = const $ return testError
                 }
    let result = unTestFixture getActions ff
    isLeft result `shouldBe` True

  it "should succeed to get Resources" $ do
    let result = unTestFixture getResources fixture
    result
      `shouldBe` Right
                   "NAME  DESCRIPTION  Mon          Tue          Wed          Thu          Fri          Sat          Sun        \n\
                   \name               00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59\n\
                   \name               00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59\n\
                   \name               00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59  00:00-23:59\n"

  it "should fail to get Resources if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getResources ff
    isLeft result `shouldBe` True

  it "should fail to get Resources if Resource load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = const $ return testError
                 }
    let result = unTestFixture getResources ff
    isLeft result `shouldBe` True

  it "should succeed to get Schedules" $ do
    let result = unTestFixture getSchedules fixture
    result
      `shouldBe` Right
                   "FROM      TO        RESOURCE  ACTION\n\
                   \01/01/00  01/01/00                  \n\
                   \01/01/00  01/01/00                  \n\
                   \01/01/00  01/01/00                  \n"

  it "should fail to get Schedules if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getSchedules ff
    isLeft result `shouldBe` True

  it "should fail to get Schedules if Schedule load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadSchedules' = const $ return testError
                 }
    let result = unTestFixture getSchedules ff
    isLeft result `shouldBe` True
