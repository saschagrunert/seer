-- | The Schedule tests
--
-- @since 0.1.0

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ScheduleSpec
  ( scheduleSpec
  ) where

import Control.Monad.TestFixture    (TestFixture
                                    ,unTestFixture)
import Control.Monad.TestFixture.TH (def
                                    ,mkFixture
                                    ,ts)
import Data.UUID                    (nil)
import Seer.Manifest                (spec)
import Seer.Schedule                (MonadSchedule
                                    ,actionID
                                    ,new
                                    ,resourceID
                                    ,start)
import Test.Tasty.Hspec             (Spec
                                    ,it
                                    ,parallel
                                    ,shouldBe)
import TestData                     (testTime
                                    ,testMetadata)

mkFixture "Fixture" [ts| MonadSchedule |]

fixture :: Fixture (TestFixture Fixture () ())
fixture = def { _newMetadata' = return testMetadata }

-- Schedule.hs related tests
-- Unit tests
scheduleSpec :: Spec
scheduleSpec = parallel $ it "should succeed to create a new Schedule" $ do
  let result = unTestFixture (new testTime nil nil) fixture
  start (spec result) `shouldBe` testTime
  resourceID (spec result) `shouldBe` nil
  actionID (spec result) `shouldBe` nil
