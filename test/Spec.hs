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
import StorageSpec           (storageSpec)
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
  units <- unitTests
  defaultMain . opts $ testGroup "Tests" [units, properties]
  where opts = localOption $ QuickCheckTests 5000

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
  actionUnitTests   <- testSpec "ActionSpec.hs" actionSpec
  configUnitTests   <- testSpec "ConfigSpec.hs" configSpec
  gitUnitTests      <- testSpec "GitSpec.hs" gitSpec
  manifestUnitTests <- testSpec "ManifestSpec.hs" manifestSpec
  resourceUnitTests <- testSpec "ResourceSpec.hs" resourceSpec
  scheduleUnitTests <- testSpec "ScheduleSpec.hs" scheduleSpec
  seerUnitTests     <- testSpec "SeerSpec.hs" seerSpec
  storageUnitTests  <- testSpec "StorageSpec.hs" storageSpec
  timeUnitTests     <- testSpec "TimeSpec.hs" timeSpec
  utilsUnitTests    <- testSpec "UtilsSpec.hs" utilsSpec
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

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup
  "Properties"
  [actionProps, configProps, resourceProps, timeProps, utilsProps]
