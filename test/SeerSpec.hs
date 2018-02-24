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
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.Either                  (isLeft
                                    ,isRight)
import Seer                         (MonadSeer
                                    ,createAction
                                    ,createResource
                                    ,createSchedule
                                    ,createStorage
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
                                    ,testResource)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)

mkFixture "Fixture" [ts| MonadSeer |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def
  { _getLine'       = return "1"
  , _list' = return $ Right [["name 1", "remote 1"], ["name 2", "remote 2"]]
  , _loadActions'   = const . return . Right $ replicate 3 testAction
  , _loadConfig'    = return $ Right testConfig
  , _loadResources' = const . return . Right $ replicate 3 testResource
  , _loadSchedules' = const . return . Right $ replicate 3 testSchedule
  , _log'           = const $ return ()
  , _newAction'     = \_ _ _ -> return $ Just testAction
  , _newConfig'     = const $ return testConfig
  , _newResource'   = \_ _ _ -> return testResource
  , _newSchedule'   = \_ _ _ -> return testSchedule
  , _newStorage'    = \_ _ -> return $ Right ()
  , _save'          = const . return $ Right ()
  , _saveActions'   = \_ _ -> return $ Right ()
  , _saveConfig'    = const . return $ Right ()
  , _saveResources' = \_ _ -> return $ Right ()
  , _saveSchedules' = \_ _ -> return $ Right ()
  , _storageExist'  = const . return $ Right True
  , _utcToLocal'    = const $ return ""
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
    result `shouldBe` Right "#  STORAGE  CREATED\n1  storage         \n"

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
                   \3  action                                \n"

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
                   "#  NAME      DESCRIPTION  Mon        Tue        Wed        Thu        Fri        Sat        Sun        CREATED\n\
                   \1  resource               0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00         \n\
                   \2  resource               0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00         \n\
                   \3  resource               0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00  0:00-0:00         \n"

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
                   "FROM           TO             RESOURCE  ACTION  CREATED\n\
                   \0-01-01  0:00  0-01-01  0:00  resource  action         \n\
                   \0-01-01  0:00  0-01-01  0:00  resource  action         \n\
                   \0-01-01  0:00  0-01-01  0:00  resource  action         \n"

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

  it "should fail to get Schedules if Action load fails" $ do
    let
      ff = def
        { _loadConfig'    = return $ Right testConfig
        , _loadSchedules' = const . return . Right $ replicate 3 testSchedule
        , _loadResources' = const . return . Right $ replicate 3 testResource
        , _loadActions'   = const $ return testError
        }
    let result = unTestFixture getSchedules ff
    isLeft result `shouldBe` True

  it "should fail to get Schedules if Resource load fails" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadSchedules' = const . return . Right $ replicate 3 testSchedule
          , _loadResources' = const $ return testError
          }
    let result = unTestFixture getSchedules ff
    isLeft result `shouldBe` True

  it "should succeed to create a new Storage without a remote" $ do
    let result = unTestFixture (createStorage "test" "") fixture
    isRight result `shouldBe` True

  it "should succeed to create a new Storage including a remote" $ do
    let result = unTestFixture (createStorage "test" "remote") fixture
    isRight result `shouldBe` True

  it "should fail to create a new Storage if internal creation fails" $ do
    let ff     = def { _newStorage' = \_ -> const $ return testError }
    let result = unTestFixture (createStorage "test" "") ff
    isLeft result `shouldBe` True

  it "should succeed to create a Action" $ do
    let result = unTestFixture (createAction "" "" "") fixture
    isRight result `shouldBe` True

  it "should fail to create a Action if entity creation fails" $ do
    let ff     = def { _newAction' = \_ _ _ -> return Nothing }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Action if entity saving fails" $ do
    let ff = def { _loadConfig'  = return $ Right testConfig
                 , _newAction'   = \_ _ _ -> return $ Just testAction
                 , _saveActions' = \_ _ -> return testError
                 }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Action if Storage save/sync fails" $ do
    let ff = def { _loadConfig'  = return $ Right testConfig
                 , _newAction'   = \_ _ _ -> return $ Just testAction
                 , _saveActions' = \_ _ -> return $ Right ()
                 , _save'        = const $ return testError
                 }
    let result = unTestFixture (createAction "" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Action if config load fails" $ do
    let ff = def { _loadConfig'  = return testError
                 , _newAction'   = \_ _ _ -> return $ Just testAction
                 , _saveActions' = \_ _ -> return $ Right ()
                 , _save'        = const . return $ Right ()
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
                 , _newResource'   = \_ _ _ -> return testResource
                 , _saveResources' = \_ _ -> return testError
                 }
    let result =
          unTestFixture (createResource "" "" ("", "", "", "", "", "", "")) ff
    isLeft result `shouldBe` True

  it "should fail to create a Resource if Storage save/sync fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _newResource'   = \_ _ _ -> return testResource
                 , _saveResources' = \_ _ -> return $ Right ()
                 , _save'          = const $ return testError
                 }
    let result =
          unTestFixture (createResource "" "" ("", "", "", "", "", "", "")) ff
    isLeft result `shouldBe` True

  it "should fail to create a Resource if config load fails" $ do
    let ff = def { _loadConfig'    = return testError
                 , _newResource'   = \_ _ _ -> return testResource
                 , _saveResources' = \_ _ -> return $ Right ()
                 , _save'          = const . return $ Right ()
                 }
    let result =
          unTestFixture (createResource "" "" ("", "", "", "", "", "", "")) ff
    isLeft result `shouldBe` True

  it "should succeed to create a Schedule" $ do
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") fixture
    isRight result `shouldBe` True

  it "should succeed to create a Schedule with only one Action and Resource"
    $ do
        let ff = def
              { _loadConfig'    = return $ Right testConfig
              , _loadResources' = const . return . Right $ pure testResource
              , _loadActions'   = const . return . Right $ pure testAction
              , _loadSchedules' = const . return $ Right []
              , _saveSchedules' = \_ _ -> return $ Right ()
              , _save'          = const . return $ Right ()
              }
        let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
        isRight result `shouldBe` True

  it "should fail to create a Schedule if config load fails" $ do
    let ff     = def { _loadConfig' = return testError }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if Resource load fails" $ do
    let ff = def { _loadConfig'    = return $ Right testConfig
                 , _loadResources' = const $ return testError
                 }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if Action load fails" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadResources' = const . return . Right $ replicate 3 testResource
          , _loadActions'   = const $ return testError
          }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if date parsing fails" $ do
    let result = unTestFixture (createSchedule "wrong date" "" "") fixture
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if no Action was found" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadResources' = const . return . Right $ replicate 3 testResource
          , _loadActions'   = const . return $ Right []
          }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if no Resource was found" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadActions'   = const . return . Right $ replicate 3 testAction
          , _loadResources' = const . return $ Right []
          }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Action is selected" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadResources' = const . return . Right $ pure testResource
          , _loadActions'   = const . return . Right $ replicate 3 testAction
          , _getLine'       = return "5"
          }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Action is selected" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadResources' = const . return . Right $ pure testResource
          , _loadActions'   = const . return . Right $ replicate 3 testAction
          , _getLine'       = return "wrong"
          }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Action is selected" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadResources' = const . return . Right $ pure testResource
          , _loadActions'   = const . return . Right $ replicate 3 testAction
          , _getLine'       = return "-2"
          }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Resource is selected" $ do
    let
      ff = def
        { _loadConfig'    = return $ Right testConfig
        , _loadActions'   = const . return . Right $ pure testAction
        , _loadResources' = const . return . Right $ replicate 3 testResource
        , _getLine'       = return "5"
        }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Resource is selected" $ do
    let
      ff = def
        { _loadConfig'    = return $ Right testConfig
        , _loadActions'   = const . return . Right $ pure testAction
        , _loadResources' = const . return . Right $ replicate 3 testResource
        , _getLine'       = return "wrong"
        }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if wrong Resource is selected" $ do
    let
      ff = def
        { _loadConfig'    = return $ Right testConfig
        , _loadActions'   = const . return . Right $ pure testAction
        , _loadResources' = const . return . Right $ replicate 3 testResource
        , _getLine'       = return "-2"
        }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if loading Schedules fails" $ do
    let
      ff = def
        { _loadConfig'    = return $ Right testConfig
        , _loadActions'   = const . return . Right $ replicate 3 testAction
        , _loadResources' = const . return . Right $ replicate 3 testResource
        , _getLine'       = return "1"
        , _loadSchedules' = const $ return testError
        }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if schedule saving fails" $ do
    let
      ff = def
        { _loadConfig'    = return $ Right testConfig
        , _loadActions'   = const . return . Right $ replicate 3 testAction
        , _loadResources' = const . return . Right $ replicate 3 testResource
        , _getLine'       = return "1"
        , _loadSchedules' = const . return . Right $ replicate 3 testSchedule
        , _saveSchedules' = \_ _ -> return testError
        }
    let result = unTestFixture (createSchedule "2018-02-02 0:10" "" "") ff
    isLeft result `shouldBe` True

  it "should fail to create a Schedule if Action is not Schedulable" $ do
    let ff = def
          { _loadConfig'    = return $ Right testConfig
          , _loadActions' = const . return . Right $ pure testActionRealDuration
          , _loadResources' = const . return . Right $ pure testResource
          , _loadSchedules' = const . return . Right $ pure testSchedule
          , _saveSchedules' = \_ _ -> return testError
          }
    let result = unTestFixture (createSchedule "0-01-01 0:00" "" "") ff
    isLeft result `shouldBe` True
