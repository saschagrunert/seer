{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module GitSpec
  ( gitSpec
  ) where

import Control.Monad.TestFixture    (unTestFixture)
import Control.Monad.TestFixture.TH (def, mkFixture, ts)
import Data.Either                  (Either (..), isLeft, isRight)
import Seer.Git                     (MonadGit, runGitCommand, runGitCommandIO)
import System.Exit                  (ExitCode (..))
import System.IO.Error              (userError)
import Test.Tasty.Hspec             (Spec, it, parallel, shouldBe, shouldReturn)

mkFixture "Fixture" [ts| MonadGit |]

-- Git.hs related tests
-- Unit tests
gitSpec :: Spec
gitSpec = parallel $ do
  it "should succeed with a real 'git status' command (runGitCommand)"
    $              isRight
    <$>            runGitCommand "status" "."
    `shouldReturn` True
  it "should fail with a real 'git test' command" $ isLeft <$> runGitCommand "test" "." `shouldReturn` True
  it "should succeed with a mocked 'git status' command (runGitCommand)" $ do
    let fixture = def { _readProcessWithExitCode' = \_ _ _ -> return (ExitSuccess, "stdOut", "")
                      , _try'                     = fmap Right
                      , _withCurrentDirectory'    = \_ a -> a
                      }
    let result = unTestFixture (runGitCommand "status" ".") fixture
    result `shouldBe` Right "stdOut"
  it "should fail with a mocked 'git test' command (runGitCommand)" $ do
    let fixture = def { _readProcessWithExitCode' = \_ _ _ -> return (ExitFailure 1, "", "stdErr")
                      , _try'                     = fmap Right
                      , _withCurrentDirectory'    = \_ a -> a
                      }
    let result = unTestFixture (runGitCommand "test" ".") fixture
    result `shouldBe` Left "stdErr"
  it "should fail with a mocked non existing 'git' executable (runGitCommand)" $ do
    let fixture = def { _readProcessWithExitCode' = \_ _ _ -> return (ExitSuccess, "", "")
                      , _try'                     = \_ -> return . Left $ userError "failure"
                      , _withCurrentDirectory'    = \_ a -> a
                      }
    let result = unTestFixture (runGitCommand "" ".") fixture
    result `shouldBe` Left "user error (failure)"
  it "should succeed with a real 'git status' command (runGitCommandIO)"
    $              isRight
    <$>            runGitCommandIO "status" "."
    `shouldReturn` True
  it "should fail with a real 'git test' command" $ isLeft <$> runGitCommandIO "test" "." `shouldReturn` True
  it "should succeed with a mocked 'git status' command (runGitCommandIO)" $ do
    let fixture = def { _readProcessWithExitCode' = \_ _ _ -> return (ExitSuccess, "stdOut", "")
                      , _try'                     = fmap Right
                      , _withCurrentDirectory'    = \_ a -> a
                      }
    let result = unTestFixture (runGitCommandIO "status" ".") fixture
    result `shouldBe` Right ()
  it "should fail with a mocked 'git test' command (runGitCommandIO)" $ do
    let fixture = def { _readProcessWithExitCode' = \_ _ _ -> return (ExitFailure 1, "", "stdErr")
                      , _try'                     = fmap Right
                      , _withCurrentDirectory'    = \_ a -> a
                      }
    let result = unTestFixture (runGitCommandIO "test" ".") fixture
    result `shouldBe` Left (userError "stdErr")
  it "should fail with a mocked non existing 'git' executable (runGitCommandIO)" $ do
    let fixture = def { _readProcessWithExitCode' = \_ _ _ -> return (ExitSuccess, "", "")
                      , _try'                     = \_ -> return . Left $ userError "failure"
                      , _withCurrentDirectory'    = \_ a -> a
                      }
    let result = unTestFixture (runGitCommandIO "" ".") fixture
    result `shouldBe` Left (userError "user error (failure)")
