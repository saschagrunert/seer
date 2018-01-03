{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (AssertionFailed(..), toException)
import Control.Monad.TestFixture (unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.Either (Either(..), isLeft, isRight)
import Seer.Git (MonadGit, runGitCommand)
import System.Exit (ExitCode(..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe, shouldReturn, testSpec)

mkFixture "MonadGitFixture" [ts| MonadGit |]

main :: IO ()
main = do
    unitTests <- testSpec "Unit Tests" gitUnitTests
    defaultMain (testGroup "Tests" [unitTests, properties])

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" []

-- Git.hs related tests
-- Unit tests
gitUnitTests :: Spec
gitUnitTests = parallel $ do
    it "should succeed with a real 'git status' command"
        $              isRight
        `fmap`         runGitCommand "status"
        `shouldReturn` True


    it "should fail with a real 'git test' command"
        $              isLeft
        `fmap`         runGitCommand "test"
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
        result `shouldBe` Right "failure"
