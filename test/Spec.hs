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
import DateParserSpec        (dateParserSpec)
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
import ViewSpec              (viewSpec)
import UtilsSpec             (utilsProps
                             ,utilsSpec)

-- The main test routine
main :: IO ()
main = do
  uTests <- unitTests
  defaultMain . opts $ testGroup "Tests" [uTests, properties]
  where opts = localOption $ QuickCheckTests 5000

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
  actionUnitTests     <- testSpec "Action" actionSpec
  configUnitTests     <- testSpec "Config" configSpec
  dateParserUnitTests <- testSpec "DateParser" dateParserSpec
  gitUnitTests        <- testSpec "Git" gitSpec
  manifestUnitTests   <- testSpec "Manifest" manifestSpec
  resourceUnitTests   <- testSpec "Resource" resourceSpec
  scheduleUnitTests   <- testSpec "Schedule" scheduleSpec
  seerUnitTests       <- testSpec "Seer" seerSpec
  storageUnitTests    <- testSpec "Storage" storageSpec
  timeUnitTests       <- testSpec "Time" timeSpec
  utilsUnitTests      <- testSpec "Utils" utilsSpec
  viewUnitTests       <- testSpec "View" viewSpec
  return $ testGroup
    "Unit Tests"
    [ actionUnitTests
    , timeUnitTests
    , configUnitTests
    , dateParserUnitTests
    , gitUnitTests
    , manifestUnitTests
    , resourceUnitTests
    , scheduleUnitTests
    , seerUnitTests
    , storageUnitTests
    , utilsUnitTests
    , viewUnitTests
    ]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup
  "Properties"
  [actionProps, configProps, resourceProps, timeProps, utilsProps]
