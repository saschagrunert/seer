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

import Control.Monad.TestFixture    (TestFixture, unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.Either                  (isLeft)
import Data.Yaml                    (ParseException (NonScalarKey))
import Seer.Storage                 (MonadStorage, list,
                                     loadActions, loadConfig, loadResources,
                                     loadSchedules, new, remove, save,
                                     saveActions, saveConfig,
                                     saveResources, saveSchedules,
                                     storageExist)
import Test.Tasty.Hspec             (Spec, it, parallel, shouldBe)
import TestData                     (testAction, testConfig, testResource,
                                     testSchedule, testError)

mkFixture "Fixture" [ts| MonadStorage |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _tryCreateDirectory'          = const . return $ Right ()
              , _tryCreateDirectoryIfMissing' = const . return $ Right ()
              , _tryEncodeFile'               = \_ _ -> return $ Right ()
              , _tryGetHomeDirectory'         = return $ Right "/"
              , _tryListDirectory' = const $ return $ Right ["1", "2", "3"]
              , _tryRemoveDirectoryRecursive' = const . return $ Right ()
              , _tryWriteFile'                = \_ _ -> return $ Right ()
              , _runGitCommand'               = \_ _ -> return $ Right ""
              , _runGitCommandIO'             = \_ _ -> return $ Right ()
              }

-- Storage.hs related tests
-- Unit tests
storageSpec :: Spec
storageSpec = parallel $ do
  it "should succeed to create a new storage without remote" $ do
    let result = unTestFixture (new "name" Nothing) fixture
    result `shouldBe` Right ()

  it "should succeed to create a new storage with remote" $ do
    let result = unTestFixture (new "name" (Just "remote")) fixture
    result `shouldBe` Right ()

  it "should fail to create a new storage if HOME directory retrieval fails"
    $ do
        let ff     = def { _tryGetHomeDirectory' = return testError }
        let result = unTestFixture (new "name" (Just "remote")) ff
        result `shouldBe` testError

  it "should fail to create a new storage if cache directory creation fails"
    $ do
        let ff = def { _tryGetHomeDirectory'         = return $ Right "/"
                     , _tryCreateDirectoryIfMissing' = const $ return testError
                     }
        let result = unTestFixture (new "name" (Just "remote")) ff
        result `shouldBe` testError

  it "should fail to create a new storage if storage directory creation fails"
    $ do
        let ff = def
              { _tryGetHomeDirectory'         = return $ Right "/"
              , _tryCreateDirectoryIfMissing' = const . return $ Right ()
              , _tryCreateDirectory'          = const $ return testError
              }
        let result = unTestFixture (new "name" (Just "remote")) ff
        result `shouldBe` testError

  it "should fail to create a new storage if .keep file creation" $ do
    let ff = def { _tryGetHomeDirectory'         = return $ Right "/"
                 , _tryCreateDirectoryIfMissing' = const . return $ Right ()
                 , _tryCreateDirectory'          = const . return $ Right ()
                 , _tryWriteFile'                = \_ _ -> return testError
                 }
    let result = unTestFixture (new "name" (Just "remote")) ff
    result `shouldBe` testError

  it "should fail to create a new storage if git command fails" $ do
    let ff = def { _tryGetHomeDirectory'         = return $ Right "/"
                 , _tryCreateDirectoryIfMissing' = const . return $ Right ()
                 , _tryCreateDirectory'          = const . return $ Right ()
                 , _tryWriteFile'                = \_ _ -> return $ Right ()
                 , _runGitCommandIO'             = \_ _ -> return testError
                 }
    let result = unTestFixture (new "name" (Just "remote")) ff
    isLeft result `shouldBe` True

  it "should succeed to remove a storage" $ do
    let result = unTestFixture (remove "name") fixture
    result `shouldBe` Right ()

  it "should fail to remove a storage if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (remove "name") ff
    result `shouldBe` testError

  it "should fail to remove a storage if directory removal fails" $ do
    let ff = def { _tryGetHomeDirectory'         = return $ Right "/"
                 , _tryRemoveDirectoryRecursive' = const $ return testError
                 }
    let result = unTestFixture (remove "name") ff
    result `shouldBe` testError

  it "should succeed to save Storage" $ do
    let result = unTestFixture (save "name") fixture
    result `shouldBe` Right ()

  it "should fail to save a storage if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (save "name") ff
    isLeft result `shouldBe` True

  it "should fail to save a storage if git command fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _runGitCommandIO'     = \_ _ -> return testError
                 }
    let result = unTestFixture (save "name") ff
    isLeft result `shouldBe` True

  it "should succeed to get existing storages" $ do
    let result = unTestFixture (storageExist "1") fixture
    result `shouldBe` Right True

  it "should fail to get storages if not existing" $ do
    let result = unTestFixture (storageExist "not_existing") fixture
    result `shouldBe` Right False

  it "should fail to get storages if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (storageExist "1") ff
    result `shouldBe` testError

  it "should fail to get storages if directory listing fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right ""
                 , _tryListDirectory'    = const $ return testError
                 }
    let result = unTestFixture (storageExist "1") ff
    result `shouldBe` testError

  it "should succeed to list storages with remotes" $ do
    let result = unTestFixture list fixture
    result `shouldBe` Right [["1", ""], ["2", ""], ["3", ""]]

  it
      "should fail to list storages with remotes if HOME directory retrieval fails"
    $ do
        let ff     = def { _tryGetHomeDirectory' = return testError }
        let result = unTestFixture list ff
        result `shouldBe` testError

  it "should fail to list storages with remotes if directory listing fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right ""
                 , _tryListDirectory'    = const $ return testError
                 }
    let result = unTestFixture list ff
    result `shouldBe` testError

  it "should succeed to save Config" $ do
    let result = unTestFixture (saveConfig testConfig) fixture
    result `shouldBe` Right ()

  it "should fail to save Config if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveConfig testConfig) ff
    result `shouldBe` testError

  it "should fail to save Config if directory creation fails" $ do
    let ff = def { _tryGetHomeDirectory'         = return $ Right "/"
                 , _tryCreateDirectoryIfMissing' = const $ return testError
                 }
    let result = unTestFixture (saveConfig testConfig) ff
    result `shouldBe` testError

  it "should fail to save Config if YAML encoding fails" $ do
    let ff = def { _tryGetHomeDirectory'         = return $ Right "/"
                 , _tryCreateDirectoryIfMissing' = const . return $ Right ()
                 , _tryEncodeFile'               = \_ _ -> return testError
                 }
    let result = unTestFixture (saveConfig testConfig) ff
    result `shouldBe` testError

  it "should succeed to load Config if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture loadConfig ff
    result `shouldBe` testError

  it "should fail to load Config if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture loadConfig ff
    isLeft result `shouldBe` True

  it "should succeed to save actions" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryEncodeFile'       = \_ _ -> return $ Right ()
                 }
    let result = unTestFixture (saveActions "name" [testAction]) ff
    result `shouldBe` Right ()

  it "should fail to save actions if YAML encoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryEncodeFile'       = \_ _ -> return testError
                 }
    let result = unTestFixture (saveActions "name" [testAction]) ff
    result `shouldBe` testError

  it "should fail to save actions if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveActions "name" [testAction]) ff
    result `shouldBe` testError

  it "should succeed to save schedules" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryEncodeFile'       = \_ _ -> return $ Right ()
                 }
    let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
    result `shouldBe` Right ()

  it "should fail to save schedules if YAML encoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryEncodeFile'       = \_ _ -> return testError
                 }
    let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
    result `shouldBe` testError

  it "should fail to save schedules if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
    result `shouldBe` testError

  it "should succeed to save resources" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryEncodeFile'       = \_ _ -> return $ Right ()
                 }
    let result = unTestFixture (saveResources "name" [testResource]) ff
    result `shouldBe` Right ()

  it "should fail to save resources if YAML encoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryEncodeFile'       = \_ _ -> return testError
                 }
    let result = unTestFixture (saveResources "name" [testResource]) ff
    result `shouldBe` testError

  it "should fail to save resources if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveResources "name" [testResource]) ff
    result `shouldBe` testError

  it "should fail to load actions if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryListYamlFiles'    = const . return $ Right [""]
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture (loadActions "name") ff
    isLeft result `shouldBe` True

  it "should fail to load actions if directory listing fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryListYamlFiles'    = const $ return testError
                 }
    let result = unTestFixture (loadActions "name") ff
    result `shouldBe` testError

  it "should fail to load actions if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (loadActions "name") ff
    result `shouldBe` testError

  it "should fail to load resources if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryListYamlFiles'    = const . return $ Right [""]
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture (loadResources "name") ff
    isLeft result `shouldBe` True

  it "should fail to load resources if directory listing fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryListYamlFiles'    = const $ return testError
                 }
    let result = unTestFixture (loadResources "name") ff
    result `shouldBe` testError

  it "should fail to load resources if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (loadResources "name") ff
    result `shouldBe` testError

  it "should fail to load schedules if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryListYamlFiles'    = const . return $ Right [""]
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture (loadSchedules "name") ff
    isLeft result `shouldBe` True

  it "should fail to load schedules if directory listing fails" $ do
    let ff = def { _tryGetHomeDirectory' = return $ Right "/"
                 , _tryListYamlFiles'    = const $ return testError
                 }
    let result = unTestFixture (loadSchedules "name") ff
    result `shouldBe` testError

  it "should fail to load schedules if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (loadSchedules "name") ff
    result `shouldBe` testError

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
