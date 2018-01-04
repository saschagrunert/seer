import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import GitSpec (gitSpec)
import TeamSpec (teamProps, teamSpec)
import UserSpec (userProps, userSpec)

-- The main test routine
main :: IO ()
main = do
    units <- unitTests
    defaultMain (testGroup "Tests" [units, properties])

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
    gitUnitTests  <- testSpec "GitSpec.hs" gitSpec
    userUnitTests <- testSpec "UserSpec.hs" userSpec
    teamUnitTests <- testSpec "TeamSpec.hs" teamSpec
    return $ testGroup "Unit Tests" [gitUnitTests, userUnitTests, teamUnitTests]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" [userProps, teamProps]
