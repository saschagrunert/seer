import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.QuickCheck (QuickCheckTests(QuickCheckTests))

import ActionSpec (actionSpec)
import AvailabilitySpec (availabilitySpec, availabilityProps)
import GitSpec (gitSpec)
import ManifestSpec (manifestSpec)
import ResourceSpec (resourceSpec)
import ScheduleSpec (scheduleSpec)
import StorageSpec (storageSpec)

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
    gitUnitTests          <- testSpec "GitSpec.hs" gitSpec
    manifestUnitTests     <- testSpec "ManifestSpec.hs" manifestSpec
    resourceUnitTests     <- testSpec "ResourceSpec.hs" resourceSpec
    scheduleUnitTests     <- testSpec "ScheduleSpec.hs" scheduleSpec
    storageUnitTests      <- testSpec "StorageSpec.hs" storageSpec
    return $ testGroup
        "Unit Tests"
        [ actionUnitTests
        , availabilityUnitTests
        , gitUnitTests
        , manifestUnitTests
        , resourceUnitTests
        , scheduleUnitTests
        , storageUnitTests
        ]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" [availabilityProps]
