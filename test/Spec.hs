import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import ActionSpec (actionProps, actionSpec)
import GitSpec (gitSpec)
import StorageSpec (storageSpec)
import ResourceSpec (resourceProps, resourceSpec)

-- The main test routine
main :: IO ()
main = do
    units <- unitTests
    defaultMain (testGroup "Tests" [units, properties])

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
    actionUnitTests   <- testSpec "ActionSpec.hs" actionSpec
    gitUnitTests      <- testSpec "GitSpec.hs" gitSpec
    storageUnitTests  <- testSpec "StorageSpec.hs" storageSpec
    resourceUnitTests <- testSpec "ResourceSpec.hs" resourceSpec
    return $ testGroup
        "Unit Tests"
        [actionUnitTests, gitUnitTests, storageUnitTests, resourceUnitTests]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" [actionProps, resourceProps]
