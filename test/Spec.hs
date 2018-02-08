-- | The main test module
--
-- @since 0.1.0
module Main where

import Test.Tasty            (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hspec      (testSpec)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests))

import ActionSpec            (actionSpec)
import AvailabilitySpec      (availabilityProps, availabilitySpec)
import ConfigSpec            (configSpec)
import GitSpec               (gitSpec)
import ManifestSpec          (manifestSpec)
import ResourceSpec          (resourceSpec)
import SeerSpec              (seerSpec)
import ScheduleSpec          (scheduleSpec)
import StorageSpec           (storageSpec)

-- The main test routine
main :: IO ()
main = do
  units <- unitTests
  defaultMain . opts $ testGroup "Tests" [units, properties]
  where opts = localOption $ QuickCheckTests 5000

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
  actionUnitTests       <- testSpec "ActionSpec.hs" actionSpec
  availabilityUnitTests <- testSpec "AvailabilitySpec.hs" availabilitySpec
  configUnitTests       <- testSpec "ConfigSpec.hs" configSpec
  gitUnitTests          <- testSpec "GitSpec.hs" gitSpec
  manifestUnitTests     <- testSpec "ManifestSpec.hs" manifestSpec
  resourceUnitTests     <- testSpec "ResourceSpec.hs" resourceSpec
  scheduleUnitTests     <- testSpec "ScheduleSpec.hs" scheduleSpec
  seerUnitTests         <- testSpec "SeerSpec.hs" seerSpec
  storageUnitTests      <- testSpec "StorageSpec.hs" storageSpec
  return $ testGroup
    "Unit Tests"
    [ actionUnitTests
    , availabilityUnitTests
    , configUnitTests
    , gitUnitTests
    , manifestUnitTests
    , resourceUnitTests
    , scheduleUnitTests
    , seerUnitTests
    , storageUnitTests
    ]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" [availabilityProps]
