-- | The Storage tests
--
-- @since 0.1.0

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

import Control.Monad.TestFixture    (TestFixture
                                    ,TestFixtureT
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.Either                  (isLeft
                                    ,isRight)
import Data.Functor.Identity        (Identity)
import Data.Yaml                    (ParseException (NonScalarKey))
import Seer.Storage                 (MonadStorage
                                    ,list
                                    ,loadActions
                                    ,loadConfig
                                    ,loadResources
                                    ,loadSchedules
                                    ,new
                                    ,remove
                                    ,removeActions
                                    ,removeResources
                                    ,removeSchedules
                                    ,save
                                    ,saveActions
                                    ,saveConfig
                                    ,saveResources
                                    ,saveSchedules
                                    ,storageExist)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import TestData                     (testAction
                                    ,testConfig
                                    ,testResource
                                    ,testSchedule
                                    ,testError)

mkFixture "Fixture" [ts| MonadStorage |]

ccrError :: a -> b -> TestFixture Fixture () () (Either IOError c)
ccrError = const . crError

ccrR :: b -> c -> d -> TestFixture Fixture () () (Either a b)
ccrR = const . crR

ccrRUnit :: b -> c -> TestFixture Fixture () () (Either a ())
ccrRUnit = const . crRUnit

crError :: a -> TestFixtureT Fixture () () Identity (Either IOError b)
crError = const $ return testError

crR :: b -> c -> TestFixture Fixture () () (Either a b)
crR = const . return . Right

crRUnit :: b -> TestFixture Fixture () () (Either a ())
crRUnit = crR ()

rRSlash :: TestFixture Fixture () () (Either a String)
rRSlash = return $ Right "/"

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _tryCreateDirectory'          = crRUnit
              , _tryCreateDirectoryIfMissing' = crRUnit
              , _tryEncodeFile'               = ccrRUnit
              , _tryGetHomeDirectory'         = rRSlash
              , _tryListDirectory'            = crR ["1", "2", "3"]
              , _tryRemoveDirectoryRecursive' = crRUnit
              , _tryWriteFile'                = ccrRUnit
              , _runGitCommand'               = ccrR ""
              , _runGitCommandIO'             = ccrRUnit
              , _tryRemoveFile'               = crRUnit
              }

-- Storage.hs related tests
-- Unit tests
storageSpec :: Spec
storageSpec = parallel $ do
  it "should succeed to create a new storage without remote" $ do
    let result = unTestFixture (new "name" Nothing) fixture
    isRight result `shouldBe` True

  it "should succeed to create a new storage with remote" $ do
    let result = unTestFixture (new "name" (Just "remote")) fixture
    isRight result `shouldBe` True

  it "should fail to create a new storage if HOME directory retrieval fails"
    $ do
        let ff     = def { _tryGetHomeDirectory' = return testError }
        let result = unTestFixture (new "name" (Just "remote")) ff
        result `shouldBe` testError

  it "should fail to create a new storage if cache directory creation fails"
    $ do
        let ff = def { _tryGetHomeDirectory'         = rRSlash
                     , _tryCreateDirectoryIfMissing' = crError
                     }
        let result = unTestFixture (new "name" (Just "remote")) ff
        result `shouldBe` testError

  it "should fail to create a new storage if storage directory creation fails"
    $ do
        let ff = def { _tryGetHomeDirectory'         = rRSlash
                     , _tryCreateDirectoryIfMissing' = crRUnit
                     , _tryCreateDirectory'          = crError
                     }
        let result = unTestFixture (new "name" (Just "remote")) ff
        result `shouldBe` testError

  it "should fail to create a new storage if .keep file creation" $ do
    let ff = def { _tryGetHomeDirectory'         = rRSlash
                 , _tryCreateDirectoryIfMissing' = crRUnit
                 , _tryCreateDirectory'          = crRUnit
                 , _tryWriteFile'                = ccrError
                 }
    let result = unTestFixture (new "name" (Just "remote")) ff
    result `shouldBe` testError

  it "should fail to create a new storage if git command fails" $ do
    let ff = def { _tryGetHomeDirectory'         = rRSlash
                 , _tryCreateDirectoryIfMissing' = crRUnit
                 , _tryCreateDirectory'          = crRUnit
                 , _tryWriteFile'                = ccrRUnit
                 , _runGitCommandIO'             = ccrError
                 }
    let result = unTestFixture (new "name" (Just "remote")) ff
    isLeft result `shouldBe` True

  it "should succeed to remove a storage" $ do
    let result = unTestFixture (remove "name") fixture
    isRight result `shouldBe` True

  it "should fail to remove a storage if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (remove "name") ff
    result `shouldBe` testError

  it "should fail to remove a storage if directory removal fails" $ do
    let ff = def { _tryGetHomeDirectory'         = rRSlash
                 , _tryRemoveDirectoryRecursive' = crError
                 }
    let result = unTestFixture (remove "name") ff
    result `shouldBe` testError

  it "should succeed to save Storage" $ do
    let result = unTestFixture (save "name") fixture
    isRight result `shouldBe` True

  it "should fail to save a storage if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (save "name") ff
    isLeft result `shouldBe` True

  it "should fail to save a storage if git command fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _runGitCommandIO' = ccrError }
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
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryListDirectory' = crError }
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
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryListDirectory' = crError }
    let result = unTestFixture list ff
    result `shouldBe` testError

  it "should succeed to save Config" $ do
    let result = unTestFixture (saveConfig testConfig) fixture
    isRight result `shouldBe` True

  it "should fail to save Config if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveConfig testConfig) ff
    result `shouldBe` testError

  it "should fail to save Config if directory creation fails" $ do
    let ff = def { _tryGetHomeDirectory'         = rRSlash
                 , _tryCreateDirectoryIfMissing' = crError
                 }
    let result = unTestFixture (saveConfig testConfig) ff
    result `shouldBe` testError

  it "should fail to save Config if YAML encoding fails" $ do
    let ff = def { _tryGetHomeDirectory'         = rRSlash
                 , _tryCreateDirectoryIfMissing' = crRUnit
                 , _tryEncodeFile'               = ccrError
                 }
    let result = unTestFixture (saveConfig testConfig) ff
    result `shouldBe` testError

  it "should succeed to load Config if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture loadConfig ff
    result `shouldBe` testError

  it "should fail to load Config if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture loadConfig ff
    isLeft result `shouldBe` True

  it "should succeed to save actions" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryEncodeFile' = ccrRUnit }
    let result = unTestFixture (saveActions "name" [testAction]) ff
    isRight result `shouldBe` True

  it "should fail to save actions if YAML encoding fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryEncodeFile' = ccrError }
    let result = unTestFixture (saveActions "name" [testAction]) ff
    result `shouldBe` testError

  it "should fail to save actions if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveActions "name" [testAction]) ff
    result `shouldBe` testError

  it "should succeed to save schedules" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryEncodeFile' = ccrRUnit }
    let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
    isRight result `shouldBe` True

  it "should fail to save schedules if YAML encoding fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryEncodeFile' = ccrError }
    let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
    result `shouldBe` testError

  it "should fail to save schedules if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveSchedules "name" [testSchedule]) ff
    result `shouldBe` testError

  it "should succeed to save resources" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryEncodeFile' = ccrRUnit }
    let result = unTestFixture (saveResources "name" [testResource]) ff
    isRight result `shouldBe` True

  it "should fail to save resources if YAML encoding fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryEncodeFile' = ccrError }
    let result = unTestFixture (saveResources "name" [testResource]) ff
    result `shouldBe` testError

  it "should fail to save resources if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (saveResources "name" [testResource]) ff
    result `shouldBe` testError

  it "should fail to load actions if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash
                 , _tryListYamlFiles'    = crR [""]
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture (loadActions "name") ff
    isLeft result `shouldBe` True

  it "should fail to load actions if directory listing fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryListYamlFiles' = crError }
    let result = unTestFixture (loadActions "name") ff
    result `shouldBe` testError

  it "should fail to load actions if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (loadActions "name") ff
    result `shouldBe` testError

  it "should fail to load resources if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash
                 , _tryListYamlFiles'    = crR [""]
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture (loadResources "name") ff
    isLeft result `shouldBe` True

  it "should fail to load resources if directory listing fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryListYamlFiles' = crError }
    let result = unTestFixture (loadResources "name") ff
    result `shouldBe` testError

  it "should fail to load resources if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (loadResources "name") ff
    result `shouldBe` testError

  it "should fail to load schedules if YAML decoding fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash
                 , _tryListYamlFiles'    = crR [""]
                 , _decodeFileEither'    = const . return $ Left NonScalarKey
                 }
    let result = unTestFixture (loadSchedules "name") ff
    isLeft result `shouldBe` True

  it "should fail to load schedules if directory listing fails" $ do
    let ff =
          def { _tryGetHomeDirectory' = rRSlash, _tryListYamlFiles' = crError }
    let result = unTestFixture (loadSchedules "name") ff
    result `shouldBe` testError

  it "should fail to load schedules if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (loadSchedules "name") ff
    result `shouldBe` testError

  it "should succeed to save and load all entities (without Mock)" $ do
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

  it "should succeed to remove an Action" $ do
    let result = unTestFixture (removeActions "" [testAction]) fixture
    isRight result `shouldBe` True

  it "should fail to remove an Action if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (removeActions "" [testAction]) ff
    result `shouldBe` testError

  it "should fail to remove an Action if file removal fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash, _tryRemoveFile' = crError }
    let result = unTestFixture (removeActions "" [testAction]) ff
    result `shouldBe` testError

  it "should succeed to remove an Resource" $ do
    let result = unTestFixture (removeResources "" [testResource]) fixture
    isRight result `shouldBe` True

  it "should fail to remove an Resource if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (removeResources "" [testResource]) ff
    result `shouldBe` testError

  it "should fail to remove an Resource if file removal fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash, _tryRemoveFile' = crError }
    let result = unTestFixture (removeResources "" [testResource]) ff
    result `shouldBe` testError

  it "should succeed to remove an Schedule" $ do
    let result = unTestFixture (removeSchedules "" [testSchedule]) fixture
    isRight result `shouldBe` True

  it "should fail to remove an Schedule if HOME directory retrieval fails" $ do
    let ff     = def { _tryGetHomeDirectory' = return testError }
    let result = unTestFixture (removeSchedules "" [testSchedule]) ff
    result `shouldBe` testError

  it "should fail to remove an Schedule if file removal fails" $ do
    let ff = def { _tryGetHomeDirectory' = rRSlash, _tryRemoveFile' = crError }
    let result = unTestFixture (removeSchedules "" [testSchedule]) ff
    result `shouldBe` testError
