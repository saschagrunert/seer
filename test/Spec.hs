-- | The main test module
--
-- @since 0.1.0

module Main
  ( main
  ) where

import ActionSpec            (actionProps
                             ,actionSpec)
import ConfigSpec            (configProps
                             ,configSpec)
import GitSpec               (gitSpec)
import ManifestSpec          (manifestSpec)
import ResourceSpec          (resourceProps
                             ,resourceSpec)
import ScheduleSpec          (scheduleSpec)
import SeerSpec              (seerSpec)
import StorageSpec           (storageSpec
                             ,storageModuleSpec)
import Test.Tasty            (TestTree
                             ,defaultMain
                             ,localOption
                             ,testGroup)
import Test.Tasty.Hspec      (testSpec)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests))
import TimeSpec              (timeProps
                             ,timeSpec)
import UtilsSpec             (utilsProps
                             ,utilsSpec)

-- The main test routine
main :: IO ()
main = do
  uTests <- unitTests
  mTests <- moduleTests
  defaultMain . opts $ testGroup "Tests" [uTests, properties, mTests]
  where opts = localOption $ QuickCheckTests 5000

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
  actionUnitTests   <- testSpec "Action" actionSpec
  configUnitTests   <- testSpec "Config" configSpec
  gitUnitTests      <- testSpec "Git" gitSpec
  manifestUnitTests <- testSpec "Manifest" manifestSpec
  resourceUnitTests <- testSpec "Resource" resourceSpec
  scheduleUnitTests <- testSpec "Schedule" scheduleSpec
  seerUnitTests     <- testSpec "Seer" seerSpec
  storageUnitTests  <- testSpec "Storage" storageSpec
  timeUnitTests     <- testSpec "Time" timeSpec
  utilsUnitTests    <- testSpec "Utils" utilsSpec
  return $ testGroup
    "Unit Tests"
    [ actionUnitTests
    , timeUnitTests
    , configUnitTests
    , gitUnitTests
    , manifestUnitTests
    , resourceUnitTests
    , scheduleUnitTests
    , seerUnitTests
    , storageUnitTests
    , utilsUnitTests
    ]

moduleTests :: IO TestTree
moduleTests = do
  storageModuleTests <- testSpec "Storage" storageModuleSpec
  return $ testGroup "Module Tests" [storageModuleTests]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup
  "Properties"
  [actionProps, configProps, resourceProps, timeProps, utilsProps]
