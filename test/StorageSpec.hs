{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module StorageSpec
  ( storageSpec
  ) where

import           Control.Monad.TestFixture    (TestFixture, unTestFixture)
import           Control.Monad.TestFixture.TH (def, mkFixture, ts)
import           Data.Either                  (isLeft)
import           Data.Time.Calendar           (fromGregorian)
import           Data.Time.Clock              (UTCTime (UTCTime, utctDay, utctDayTime))
import           Data.UUID                    (nil)
import           Data.Yaml                    (ParseException (NonScalarKey))
import qualified Seer.Action                  as A (Action, ActionSpec (ActionSpec, description, duration, name),
                                                    Duration (Duration))
import           Seer.Availability            (weekAvailable)
import qualified Seer.Config                  as C (Config, ConfigSpec (ConfigSpec, storage))
import           Seer.Manifest                (ApiVersion (V1), Manifest (Manifest, apiVersion, kind, metadata, spec),
                                               Metadata (Metadata, creationTimestamp, uid),
                                               ResourceKind (Action, Config, Resource, Schedule))
import qualified Seer.Resource                as R (Resource, ResourceSpec (ResourceSpec, availabilities, description, name))
import qualified Seer.Schedule                as S (Schedule, ScheduleSpec (ScheduleSpec, actionID, from, resourceID, to))
import           Seer.Storage                 (MonadStorage, list,
                                               loadActions, loadConfig, loadResources,
                                               loadSchedules, new, remove, save,
                                               saveActions, saveConfig,
                                               saveResources, saveSchedules)
import           System.IO.Error              (userError)
import           Test.Tasty.Hspec             (Spec, it, parallel, shouldBe)

mkFixture "Fixture" [ts| MonadStorage |]

-- Test data
fixture :: Fixture (TestFixture Fixture () ())
fixture =
  def
  { _tryCreateDirectory' = \_ -> return $ Right ()
  , _tryCreateDirectoryIfMissing' = \_ -> return $ Right ()
  , _tryEncodeFile' = \_ _ -> return $ Right ()
  , _tryGetHomeDirectory' = return $ Right "/"
  , _tryListDirectory' = \_ -> return $ Right ["1", "2", "3"]
  , _tryRemoveDirectoryRecursive' = \_ -> return $ Right ()
  , _tryWriteFile' = \_ _ -> return $ Right ()
  , _runGitCommand' = \_ _ -> return $ Right ""
  , _runGitCommandIO' = \_ _ -> return $ Right ()
  }

testAction :: A.Action
testAction =
  Manifest
  { apiVersion = V1
  , kind = Action
  , metadata = testMetadata
  , spec =
      A.ActionSpec
      {A.name = "name", A.description = Nothing, A.duration = A.Duration 0}
  }

testConfig :: C.Config
testConfig =
  Manifest
  { apiVersion = V1
  , kind = Config
  , metadata = testMetadata
  , spec = C.ConfigSpec {C.storage = Just "name"}
  }

testResource :: R.Resource
testResource =
  Manifest
  { apiVersion = V1
  , kind = Resource
  , metadata = testMetadata
  , spec =
      R.ResourceSpec
      { R.name = "name"
      , R.description = Nothing
      , R.availabilities = weekAvailable
      }
  }

testSchedule :: S.Schedule
testSchedule =
  Manifest
  { apiVersion = V1
  , kind = Schedule
  , metadata = testMetadata
  , spec =
      S.ScheduleSpec
      {S.from = testTime, S.to = testTime, S.resourceID = nil, S.actionID = nil}
  }

testMetadata :: Metadata
testMetadata = Metadata {creationTimestamp = testTime, uid = nil}

testTime :: UTCTime
testTime = UTCTime {utctDay = fromGregorian 0 0 0, utctDayTime = 0}

-- Evaluates to the standard user error for testing
stdUserError :: Either IOError b
stdUserError = Left $ userError "failure"

-- Storage.hs related tests
-- Unit tests
storageSpec :: Spec
storageSpec =
  parallel $ do
    it "should succeed to create a new storage without remote" $ do
      let result = unTestFixture (new "name" Nothing) fixture
      result `shouldBe` Right ()

    it "should succeed to create a new storage with remote" $ do
      let result = unTestFixture (new "name" (Just "remote")) fixture
      result `shouldBe` Right ()

    it "should fail to create a new storage if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (new "name" (Just "remote")) ff
      result `shouldBe` stdUserError

    it "should fail to create a new storage if cache directory creation fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryCreateDirectoryIfMissing' = \_ -> return stdUserError
            }
      let result = unTestFixture (new "name" (Just "remote")) ff
      result `shouldBe` stdUserError

    it "should fail to create a new storage if storage directory creation fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryCreateDirectoryIfMissing' = \_ -> return $ Right ()
            , _tryCreateDirectory' = \_ -> return stdUserError
            }
      let result = unTestFixture (new "name" (Just "remote")) ff
      result `shouldBe` stdUserError

    it "should fail to create a new storage if .keep file creation" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryCreateDirectoryIfMissing' = \_ -> return $ Right ()
            , _tryCreateDirectory' = \_ -> return $ Right ()
            , _tryWriteFile' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (new "name" (Just "remote")) ff
      result `shouldBe` stdUserError

    it "should fail to create a new storage if git command fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryCreateDirectoryIfMissing' = \_ -> return $ Right ()
            , _tryCreateDirectory' = \_ -> return $ Right ()
            , _tryWriteFile' = \_ _ -> return $ Right ()
            , _runGitCommandIO' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (new "name" (Just "remote")) ff
      isLeft result `shouldBe` True

    it "should succeed to remove a storage" $ do
      let result = unTestFixture (remove "name") fixture
      result `shouldBe` Right ()

    it "should fail to remove a storage if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (remove "name") ff
      result `shouldBe` stdUserError

    it "should fail to remove a storage if directory removal fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryRemoveDirectoryRecursive' = \_ -> return stdUserError
            }
      let result = unTestFixture (remove "name") ff
      result `shouldBe` stdUserError

    it "should succeed to save Storage" $ do
      let result = unTestFixture (save "name") fixture
      result `shouldBe` Right ()

    it "should fail to save a storage if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (save "name") ff
      isLeft result `shouldBe` True

    it "should fail to save a storage if git command fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _runGitCommandIO' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (save "name") ff
      isLeft result `shouldBe` True

    it "should succeed to list storages with remotes" $ do
      let result = unTestFixture list fixture
      result `shouldBe` Right [["1", ""], ["2", ""], ["3", ""]]

    it "should fail to list storages with remotes if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture list ff
      result `shouldBe` stdUserError

    it "should fail to list storages with remotes if directory listing fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right ""
            , _tryListDirectory' = \_ -> return stdUserError
            }
      let result = unTestFixture list ff
      result `shouldBe` stdUserError

    it "should succeed to save Config" $ do
      let result = unTestFixture (saveConfig testConfig) fixture
      result `shouldBe` Right ()

    it "should fail to save Config if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (saveConfig testConfig) ff
      result `shouldBe` stdUserError

    it "should fail to save Config if directory creation fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryCreateDirectoryIfMissing' = \_ -> return stdUserError
            }
      let result = unTestFixture (saveConfig testConfig) ff
      result `shouldBe` stdUserError

    it "should fail to save Config if YAML encoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryCreateDirectoryIfMissing' = \_ -> return $ Right ()
            , _tryEncodeFile' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (saveConfig testConfig) ff
      result `shouldBe` stdUserError

    it "should succeed to load Config if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture loadConfig ff
      result `shouldBe` stdUserError

    it "should fail to load Config if YAML decoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _decodeFileEither' = \_ -> return $ Left NonScalarKey
            }
      let result = unTestFixture loadConfig ff
      isLeft result `shouldBe` True

    it "should succeed to save actions" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryEncodeFile' = \_ _ -> return $ Right ()
            }
      let result = unTestFixture (saveActions "name" [testAction]) ff
      result `shouldBe` Right ()

    it "should fail to save actions if YAML encoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryEncodeFile' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (saveActions "name" [testAction]) ff
      result `shouldBe` stdUserError

    it "should fail to save actions if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (saveActions "name" [testAction]) ff
      result `shouldBe` stdUserError

    it "should succeed to save schedules" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryEncodeFile' = \_ _ -> return $ Right ()
            }
      let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
      result `shouldBe` Right ()

    it "should fail to save schedules if YAML encoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryEncodeFile' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
      result `shouldBe` stdUserError

    it "should fail to save schedules if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
      result `shouldBe` stdUserError

    it "should succeed to save resources" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryEncodeFile' = \_ _ -> return $ Right ()
            }
      let result = unTestFixture (saveResources "name" [testResource]) ff
      result `shouldBe` Right ()

    it "should fail to save resources if YAML encoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryEncodeFile' = \_ _ -> return stdUserError
            }
      let result = unTestFixture (saveResources "name" [testResource]) ff
      result `shouldBe` stdUserError

    it "should fail to save resources if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (saveResources "name" [testResource]) ff
      result `shouldBe` stdUserError

    it "should fail to load actions if YAML decoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryListYamlFiles' = \_ -> return $ Right [""]
            , _decodeFileEither' = \_ -> return $ Left NonScalarKey
            }
      let result = unTestFixture (loadActions "name") ff
      isLeft result `shouldBe` True

    it "should fail to load actions if directory listing fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryListYamlFiles' = \_ -> return stdUserError
            }
      let result = unTestFixture (loadActions "name") ff
      result `shouldBe` stdUserError

    it "should fail to load actions if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (loadActions "name") ff
      result `shouldBe` stdUserError

    it "should fail to load resources if YAML decoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryListYamlFiles' = \_ -> return $ Right [""]
            , _decodeFileEither' = \_ -> return $ Left NonScalarKey
            }
      let result = unTestFixture (loadResources "name") ff
      isLeft result `shouldBe` True

    it "should fail to load resources if directory listing fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryListYamlFiles' = \_ -> return stdUserError
            }
      let result = unTestFixture (loadResources "name") ff
      result `shouldBe` stdUserError

    it "should fail to load resources if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (loadResources "name") ff
      result `shouldBe` stdUserError

    it "should fail to load schedules if YAML decoding fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryListYamlFiles' = \_ -> return $ Right [""]
            , _decodeFileEither' = \_ -> return $ Left NonScalarKey
            }
      let result = unTestFixture (loadSchedules "name") ff
      isLeft result `shouldBe` True

    it "should fail to load schedules if directory listing fails" $ do
      let ff =
            def
            { _tryGetHomeDirectory' = return $ Right "/"
            , _tryListYamlFiles' = \_ -> return stdUserError
            }
      let result = unTestFixture (loadSchedules "name") ff
      result `shouldBe` stdUserError

    it "should fail to load schedules if HOME directory retrieval fails" $ do
      let ff = def {_tryGetHomeDirectory' = return stdUserError}
      let result = unTestFixture (loadSchedules "name") ff
      result `shouldBe` stdUserError

    it "should succeed to save and load all entities" $ do
      let t = "unittest"
      a <- new t Nothing
      b <- saveActions t [testAction]
      c <- saveResources t [testResource]
      d <- saveSchedules t [testSchedule]
      e <- loadActions t
      f <- loadResources t
      g <- loadSchedules t
      h <- saveConfig testConfig
      i <- loadConfig
      _ <- remove t
      a `shouldBe` Right ()
      b `shouldBe` Right ()
      c `shouldBe` Right ()
      d `shouldBe` Right ()
      e `shouldBe` Right [testAction]
      f `shouldBe` Right [testResource]
      g `shouldBe` Right [testSchedule]
      h `shouldBe` Right ()
      i `shouldBe` Right testConfig
