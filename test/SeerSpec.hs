-- | The Seer tests
--
-- @since 0.1.0

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

import Control.Monad.TestFixture    (TestFixture
                                    ,TestFixtureT
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.Either                  (isLeft
                                    ,isRight)
import Data.Functor.Identity        (Identity)
import Seer                         (MonadSeer
                                    ,createAction
                                    ,createResource
                                    ,createSchedule
                                    ,createStorage
                                    ,deleteAction
                                    ,deleteResource
                                    ,deleteSchedule
                                    ,deleteStorage
                                    ,getActions
                                    ,getConfig
                                    ,getResources
                                    ,getSchedules
                                    ,getStorages
                                    ,setDefaultStorage)
import TestData                     (testAction
                                    ,testActionRealDuration
                                    ,testConfig
                                    ,testError
                                    ,testSchedule
                                    ,testTime
                                    ,testTime2000
                                    ,testResource)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)

mkFixture "Fixture" [ts| MonadSeer |]

cr :: a -> b -> TestFixture Fixture () () a
cr = const . return

crR :: b -> c -> TestFixture Fixture () () (Either a b)
crR = cr . Right

crRUnit :: b -> TestFixture Fixture () () (Either a ())
crRUnit = crR ()

ccrRUnit :: b -> c -> TestFixture Fixture () () (Either a ())
ccrRUnit = const . crRUnit

ccrError :: a -> b -> TestFixture Fixture () () (Either IOError c)
ccrError = const . crError

crError :: a -> TestFixtureT Fixture () () Identity (Either IOError b)
crError = const $ return testError

const2r :: a -> b -> b2 -> TestFixture Fixture () () a
const2r = const . cr

const3r :: a -> b -> c -> d -> TestFixture Fixture () () a
const3r = const . const2r

fixture :: Fixture (TestFixture Fixture () ())
fixture = def
  { _getCurrentTime'  = return testTime
  , _getLine'         = return "1"
  , _list' = return $ Right [["name 1", "remote 1"], ["name 2", "remote 2"]]
  , _loadActions'     = crR $ replicate 3 testAction
  , _loadConfig'      = return $ Right testConfig
  , _loadResources'   = crR $ replicate 3 testResource
  , _loadSchedules'   = crR $ replicate 3 testSchedule
  , _log'             = cr ()
  , _newAction'       = const3r $ Just testAction
  , _newConfig'       = cr testConfig
  , _newResource'     = const3r testResource
  , _newSchedule'     = const3r testSchedule
  , _newStorage'      = ccrRUnit
  , _removeActions'   = ccrRUnit
  , _removeResources' = ccrRUnit
  , _removeSchedules' = ccrRUnit
  , _removeStorage'   = crRUnit
  , _save'            = crRUnit
  , _saveActions'     = ccrRUnit
  , _saveConfig'      = crRUnit
  , _saveResources'   = ccrRUnit
  , _saveSchedules'   = ccrRUnit
  , _storageExist'    = cr $ Right True
  , _utcToLocal'      = cr ""
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
                   \name 2  remote 2"

  it "should fail to get Storages if listing fails" $ do
    let ff     = def { _list' = return testError }
    let result = unTestFixture getStorages ff
    isLeft result `shouldBe` True

  it "should succeed to set the default Storage" $ do
    let result = unTestFixture (setDefaultStorage "") fixture
    isRight result `shouldBe` True

  it "should fail to set the default Storage if not existing" $ do
    let ff     = def { _storageExist' = crR False }
    let result = unTestFixture (setDefaultStorage "") ff
    isLeft result `shouldBe` True

  it "should fail to set the default Storage if Config save failed" $ do
    let ff = def { _storageExist' = crR True
                 , _newConfig'    = cr testConfig
                 , _saveConfig'   = crError
                 }
    let result = unTestFixture (setDefaultStorage "") ff
    isLeft result `shouldBe` True

  it "should fail to set the default Storage if Storage retrieval failed" $ do
    let ff     = def { _storageExist' = crError }
    let result = unTestFixture (setDefaultStorage "") ff
    isLeft result `shouldBe` True

  it "should succeed to get the Config" $ do
    let result = unTestFixture getConfig fixture
    result `shouldBe` Right "#  STORAGE  CREATED\n1  storage"

  it "should fail to get the Config if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getConfig ff
    isLeft result `shouldBe` True

  it "should succeed to get Actions" $ do
    let result = unTestFixture getActions fixture
    result
      `shouldBe` Right
                   "#  NAME    DESCRIPTION  DURATION  CREATED\n\
                   \1  action                                \n\
                   \2  action                                \n\
                   \3  action"

  it "should fail to get Actions if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getActions ff
    isLeft result `shouldBe` True

  it "should fail to get Actions if Action load fails" $ do
    let
      ff = def { _loadConfig'  = return $ Right testConfig
               , _loadActions' = crError
               }
    let result = unTestFixture getActions ff
    isLeft result `shouldBe` True

  it "should succeed to get Resources" $ do
    let result = unTestFixture getResources fixture
    result
      `shouldBe` Right
                   "#  NAME      DESCRIPTION  Mon        Tue        Wed        Thu        Fri        Sat        Sun        CREATED\n\
                   \1  resource               0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00         \n\
                   \2  resource               0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00         \n\
                   \3  resource               0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00"

  it "should fail to get Resources if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture getResources ff
    isLeft result `shouldBe` True

  it "should fail to get Resources if Resource load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crError
                 }
    let result = unTestFixture getResources ff
    isLeft result `shouldBe` True

  it "should succeed to get Schedules" $ do
    let result = unTestFixture (getSchedules True) fixture
    result
      `shouldBe` Right
                   "#  FROM           TO             DURATION  RESOURCE  ACTION  CREATED\n\
                   \1  0-01-01  0:00  0-01-01  0:00            resource  action         \n\
                   \2  0-01-01  0:00  0-01-01  0:00            resource  action         \n\
                   \3  0-01-01  0:00  0-01-01  0:00            resource  action"

  it "should fail to get Schedules if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture (getSchedules True) ff
    isLeft result `shouldBe` True

  it "should fail to get Schedules if Schedule load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadSchedules' = crError
                 }
    let result = unTestFixture (getSchedules True) ff
    isLeft result `shouldBe` True

  it "should fail to get Schedules if Action load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadSchedules' = crR $ replicate 3 testSchedule
                 , _loadResources' = crR $ replicate 3 testResource
                 , _loadActions'   = crError
                 }
    let result = unTestFixture (getSchedules True) ff
    isLeft result `shouldBe` True

  it "should fail to get Schedules if Resource load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadSchedules' = crR $ replicate 3 testSchedule
                 , _loadResources' = crError
                 }
    let result = unTestFixture (getSchedules True) ff
    isLeft result `shouldBe` True

  it "should succeed to get Schedules if no upcoming Schedule is available" $ do
    let ff = def { _loadConfig'     = return $ Right testConfig
                 , _loadSchedules'  = crR $ replicate 3 testSchedule
                 , _loadResources'  = crR $ replicate 3 testResource
                 , _loadActions'    = crR $ replicate 3 testAction
                 , _utcToLocal'     = cr ""
                 , _getCurrentTime' = return testTime2000
                 }
    let result = unTestFixture (getSchedules False) ff
    result `shouldBe` Right "✗ Nothing found"

  it "should succeed to create a new Storage without a remote" $ do
    let result = unTestFixture (createStorage "test" "") fixture
    isRight result `shouldBe` True

  it "should succeed to create a new Storage including a remote" $ do
    let result = unTestFixture (createStorage "test" "remote") fixture
    isRight result `shouldBe` True

  it "should fail to create a new Storage if internal creation fails" $ do
    let ff     = def { _newStorage' = ccrError }
    let result = unTestFixture (createStorage "test" "") ff
    isLeft result `shouldBe` True

  it "should succeed to create a Action" $ do
    let result = unTestFixture (createAction "" "" "") fixture
    isRight result `shouldBe` True

  it "should fail to create a Action if entity creation fails" $ do
    let ff     = def { _newAction' = const3r Nothing }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Action if entity saving fails" $ do
    let ff = def { _loadConfig'  = return $ Right testConfig
                 , _newAction'   = const3r $ Just testAction
                 , _saveActions' = const2r testError
                 }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Action if Storage save/sync fails" $ do
    let ff = def { _loadConfig'  = return $ Right testConfig
                 , _newAction'   = const3r $ Just testAction
                 , _saveActions' = const2r $ Right ()
                 , _save'        = crError
                 }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Action if config load fails" $ do
    let ff = def { _loadConfig'  = return testError
                 , _newAction'   = const3r $ Just testAction
                 , _saveActions' = const2r $ Right ()
                 , _save'        = crRUnit
                 }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should succeed to create a Resource" $ do
    let result = unTestFixture
          (createResource "" "" ("", "", "", "", "", "", ""))
          fixture
    isRight result `shouldBe` True

  it "should fail to create a Resource if entity saving fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _newResource'   = const3r testResource
                 , _saveResources' = const2r testError
                 }
    let result =
          unTestFixture (createResource "" "" ("", "", "", "", "", "", "")) ff
    isLeft result `shouldBe` True

  it "should fail to create a Resource if Storage save/sync fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _newResource'   = const3r testResource
                 , _saveResources' = const2r $ Right ()
                 , _save'          = crError
                 }
    let result =
          unTestFixture (createResource "" "" ("", "", "", "", "", "", "")) ff
    isLeft result `shouldBe` True

  it "should fail to create a Resource if config load fails" $ do
    let ff = def { _loadConfig'    = return testError
                 , _newResource'   = const3r testResource
                 , _saveResources' = const2r $ Right ()
                 , _save'          = crRUnit
                 }
    let result =
          unTestFixture (createResource "" "" ("", "", "", "", "", "", "")) ff
    isLeft result `shouldBe` True

  it "should succeed to create a Schedule" $ do
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") fixture
    isRight result `shouldBe` True

  it "should succeed to create a Schedule with only one Action and Resource"
    $ do
        let ff = def { _loadConfig'    = return $ Right testConfig
                     , _loadResources' = crR $ pure testResource
                     , _loadActions'   = crR $ pure testAction
                     , _loadSchedules' = crR []
                     , _saveSchedules' = const2r $ Right ()
                     , _save'          = crRUnit
                     }
        let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
        isRight result `shouldBe` True

  it "should fail to create a Schedule if config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if Resource load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crError
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if Action load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crR $ replicate 3 testResource
                 , _loadActions'   = crError
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if date parsing fails" $ do
    let result = unTestFixture (createSchedule "wrong date" "" "") fixture
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if no Action was found" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crR $ replicate 3 testResource
                 , _loadActions'   = crR []
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if no Resource was found" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ replicate 3 testAction
                 , _loadResources' = crR []
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Action is selected" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crR $ pure testResource
                 , _loadActions'   = crR $ replicate 3 testAction
                 , _getLine'       = return "5"
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Action is selected" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crR $ pure testResource
                 , _loadActions'   = crR $ replicate 3 testAction
                 , _getLine'       = return "wrong"
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Action is selected" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crR $ pure testResource
                 , _loadActions'   = crR $ replicate 3 testAction
                 , _getLine'       = return "-2"
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Resource is selected" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ pure testAction
                 , _loadResources' = crR $ replicate 3 testResource
                 , _getLine'       = return "5"
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Resource is selected" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ pure testAction
                 , _loadResources' = crR $ replicate 3 testResource
                 , _getLine'       = return "wrong"
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Resource is selected" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ pure testAction
                 , _loadResources' = crR $ replicate 3 testResource
                 , _getLine'       = return "-2"
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if loading Schedules fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ replicate 3 testAction
                 , _loadResources' = crR $ replicate 3 testResource
                 , _getLine'       = return "1"
                 , _loadSchedules' = crError
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if schedule saving fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ replicate 3 testAction
                 , _loadResources' = crR $ replicate 3 testResource
                 , _getLine'       = return "1"
                 , _loadSchedules' = crR $ replicate 3 testSchedule
                 , _saveSchedules' = ccrError
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if Action is not Schedulable" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ pure testActionRealDuration
                 , _loadResources' = crR $ pure testResource
                 , _loadSchedules' = crR $ pure testSchedule
                 , _saveSchedules' = ccrError
                 }
    let result = unTestFixture (createSchedule "0-01-01 0:00" "" "") ff
    isLeft result `shouldBe` True

  it "should succeed to remove a Storage" $ do
    let result = unTestFixture (deleteStorage "") fixture
    isRight result `shouldBe` True

  it "should fail to remove a Storage if internal removal fails" $ do
    let ff     = def { _removeStorage' = crError }
    let result = unTestFixture (deleteStorage "") ff
    isLeft result `shouldBe` True

  it "should succeed to remove a Schedule" $ do
    let result = unTestFixture (deleteSchedule "0-01-01  0:00") fixture
    isRight result `shouldBe` True

  it "should fail to remove a Schedule if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture (deleteSchedule "0-01-01  0:00") ff
    isLeft result `shouldBe` True

  it "should fail to remove a Schedule if Schedules load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadSchedules' = crError
                 }
    let result = unTestFixture (deleteSchedule "0-01-01  0:00") ff
    isLeft result `shouldBe` True

  it "should fail to remove a Schedule if nothing found" $ do
    let result = unTestFixture (deleteSchedule "") fixture
    isLeft result `shouldBe` True

  it "should fail to remove a Schedule if internal removal fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = const2r testError
                 }
    let result = unTestFixture (deleteSchedule "0-01-01  0:00") ff
    isLeft result `shouldBe` True

  it "should fail to remove a Schedule if Storage save/sync fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = const2r $ Right ()
                 , _save'            = crError
                 }
    let result = unTestFixture (deleteSchedule "0-01-01  0:00") ff
    isLeft result `shouldBe` True

  it "should succeed to remove an Action" $ do
    let result = unTestFixture (deleteAction "") fixture
    isRight result `shouldBe` True

  it "should fail to remove an Action if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Action if Actions load fails" $ do
    let
      ff = def { _loadConfig'  = return $ Right testConfig
               , _loadActions' = crError
               }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Action if nothing found" $ do
    let
      ff =
        def { _loadConfig' = return $ Right testConfig, _loadActions' = crR [] }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Action if Schedule load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadActions'   = crR $ pure testAction
                 , _loadSchedules' = crError
                 }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Action if Schedule removal fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadActions'     = crR $ pure testAction
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = const2r testError
                 }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Action if internal removal fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadActions'     = crR $ pure testAction
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = ccrRUnit
                 , _save'            = crRUnit
                 , _removeActions'   = const2r testError
                 }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Action if Storage save/sync fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadActions'     = crR $ pure testAction
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = ccrRUnit
                 , _save'            = crError
                 }
    let result = unTestFixture (deleteAction "") ff
    isLeft result `shouldBe` True

  it "should succeed to remove an Resource" $ do
    let result = unTestFixture (deleteResource "") fixture
    isRight result `shouldBe` True

  it "should fail to remove an Resource if Config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Resource if Resources load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crError
                 }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Resource if nothing found" $ do
    let
      ff = def { _loadConfig'    = return $ Right testConfig
               , _loadResources' = crR []
               }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Resource if Schedule load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = crR $ pure testResource
                 , _loadSchedules' = crError
                 }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Resource if Schedule removal fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadResources'   = crR $ pure testResource
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = const2r testError
                 }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Resource if internal removal fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadResources'   = crR $ pure testResource
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = ccrRUnit
                 , _save'            = crRUnit
                 , _removeResources' = const2r testError
                 }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True

  it "should fail to remove an Resource if Storage save/sync fails" $ do
    let ff = def { _loadConfig'      = return $ Right testConfig
                 , _loadResources'   = crR $ pure testResource
                 , _loadSchedules'   = crR $ pure testSchedule
                 , _removeSchedules' = ccrRUnit
                 , _save'            = crError
                 }
    let result = unTestFixture (deleteResource "") ff
    isLeft result `shouldBe` True
