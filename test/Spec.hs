import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import ActionSpec (actionProps, actionSpec)
import GitSpec (gitSpec)
import StorageSpec (storageSpec)
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
    actionUnitTests  <- testSpec "ActionSpec.hs" actionSpec
    gitUnitTests     <- testSpec "GitSpec.hs" gitSpec
    storageUnitTests <- testSpec "StorageSpec.hs" storageSpec
    teamUnitTests    <- testSpec "TeamSpec.hs" teamSpec
    userUnitTests    <- testSpec "UserSpec.hs" userSpec
    return $ testGroup
        "Unit Tests"
        [ actionUnitTests
        , gitUnitTests
        , storageUnitTests
        , teamUnitTests
        , userUnitTests
        ]

-- Property tests based on quickcheck and smallcheck
properties :: TestTree
properties = testGroup "Properties" [actionProps, userProps, teamProps]
