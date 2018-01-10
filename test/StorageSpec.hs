{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module StorageSpec (storageSpec) where

import Control.Monad.TestFixture (unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.ByteString.Char8 (pack)
import Data.Either (Either(..), isLeft)
import Data.Maybe (fromJust)
import Data.Yaml (decodeEither, ParseException(NonScalarKey))
import Seer.Storage (actions, empty, load, save, resources, MonadStorage, Storage)
import System.Directory (removeFile)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe, shouldContain, shouldNotBe, shouldReturn)
import qualified Seer.Action as Action
import qualified Seer.Resource as Resource

mkFixture "MonadStorageFixture" [ts| MonadStorage |]

-- Storage.hs related tests
-- Unit tests
storageSpec :: Spec
storageSpec = parallel $ do
    it "should succeed to 'show' Storage"
        $          show [empty]
        `shouldBe` "[Storage {resources = Nothing, actions = Nothing}]"

    it "should succeed to create empty Actions" $ empty `shouldBe` empty

    it "should succeed with a real 'load' command and a real testfile" $ do
        result <- load "test/files/example.yaml"
        case result of
            Right s -> do
                s `shouldNotBe` empty
                fromJust (actions s) `shouldNotBe` Action.empty
                fromJust (resources s) `shouldNotBe` Resource.empty
            Left e -> fail $ "Failed to parse file: " ++ e

    it "should succeed with a real 'save' command" $ do
        let fn = "test/files/output.yaml"
        save empty fn
        c <- readFile fn
        c `shouldBe` "actions: null\nresources: null\n"
        removeFile fn

    it "should fail with a real 'load' command and a not existing file" $ do
        let res = decodeEither $ pack "- a" :: Either String Storage
        case res of
            Left  a -> a `shouldContain` "not an object"
            Right _ -> fail "the 'load' should fail"

    it "should fail with wrong formatted YAML"
        $              load "not_existing_file"
        `shouldReturn` Left
                           "YAML exception:\nYaml file not found: not_existing_file"

    it "should succeed with a mocked 'load'/'save' command" $ do
        let fixture = def { _decodeFileEither' = \_ -> return $ Right empty
                          , _encodeFile'       = \_ _ -> return ()
                          }
        -- Load a pseudo file
        let loadResult = unTestFixture (load "file.yaml") fixture
        loadResult `shouldBe` Right empty
        -- Save the empty storage
        let saveResult = unTestFixture (save empty "") fixture
        saveResult `shouldBe` ()

    it "should fail with a mocked 'load' command and failing 'decodeFileEither'"
        $ do
              let fixture = def
                      { _decodeFileEither' = \_ -> return $ Left NonScalarKey
                      , _encodeFile'       = \_ _ -> return ()
                      }
              let result = unTestFixture (load "file.yaml") fixture
              isLeft result `shouldBe` True
