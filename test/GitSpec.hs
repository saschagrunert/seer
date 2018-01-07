{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GitSpec (gitSpec) where

import Control.Exception (AssertionFailed(..), toException)
import Control.Monad.TestFixture (unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.Either (Either(..), isLeft, isRight)
import System.Exit (ExitCode(..))
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe, shouldReturn)
import Seer.Git (MonadGit, runGitCommand)

mkFixture "MonadGitFixture" [ts| MonadGit |]

-- Git.hs related tests
-- Unit tests
gitSpec :: Spec
gitSpec = parallel $ do
    it "should succeed with a real 'git status' command"
        $              isRight
        <$>            runGitCommand "status"
        `shouldReturn` True


    it "should fail with a real 'git test' command"
        $              isLeft
        <$>            runGitCommand "test"
        `shouldReturn` True


    it "should succeed with a mocked 'git status' command" $ do
        let fixture = def
                { _readProcessWithExitCode' = \_ _ _ ->
                    return (ExitSuccess, "stdOut", "")
                , _try'                     = fmap Right
                }
        let result = unTestFixture (runGitCommand "status") fixture
        result `shouldBe` Right "stdOut"


    it "should fail with a mocked 'git test' command" $ do
        let fixture = def
                { _readProcessWithExitCode' = \_ _ _ ->
                    return (ExitFailure 1, "", "stdErr")
                , _try'                     = fmap Right
                }
        let result = unTestFixture (runGitCommand "test") fixture
        result `shouldBe` Left "stdErr"


    it "should fail with a mocked non existing 'git' executable" $ do
        let
            fixture = def
                { _readProcessWithExitCode' = \_ _ _ ->
                    return (ExitSuccess, "", "")
                , _try'                     = \_ ->
                    return $ Left (toException (AssertionFailed "failure"))
                }
        let result = unTestFixture (runGitCommand "") fixture
        result `shouldBe` Left "failure"
