import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import GitSpec (gitSpec)
import AvailabilitySpec (availabilitySpec)

-- The main test routine
main :: IO ()
main = do
    units <- unitTests
    defaultMain (testGroup "Tests" [units, properties])

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
    gitUnitTests          <- testSpec "GitSpec.hs" gitSpec
    availabilityUnitTests <- testSpec "AvailabilitySpec.hs" availabilitySpec
    return $ testGroup "Unit Tests" [availabilityUnitTests, gitUnitTests]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" []
