module TeamSpec (
    teamProps,
    teamSpec,
) where

import Seer.Team (empty, name, newTeam, users)
import qualified Seer.User as User
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)
import Test.Tasty (testGroup, TestTree)

-- User.hs related tests
-- Unit tests
teamSpec :: Spec
teamSpec = parallel $ do
    it "should succeed to create a new empty Team"
        $          newTeam "test"
        `shouldBe` newTeam "test"

    it "should succeed to 'show' a Team"
        $          show [newTeam "test"]
        `shouldBe` "[Team {name = \"test\", users = Users (fromList [])}]"

    it "should succeed to 'show' Teams"
        $          show [empty]
        `shouldBe` "[Teams (fromList [])]"

-- Property tests
teamProps :: TestTree
teamProps = testGroup
    "TeamSpec.hs"
    [
    -- 'Team ==' test
      testProperty "team equal"
        $ \testName -> newTeam testName == newTeam testName

    -- 'Team =/' test
    , testProperty "team not equal"
        $ \t1 t2 -> t1 /= t2 ==> newTeam t1 /= newTeam t2

    -- Team 'show' test
    , testProperty "team show" $ \testName ->
        show (newTeam testName)
            == "Team {name = \""
            ++ testName
            ++ "\", users = Users (fromList [])}"

   -- Team 'name' test
    , testProperty "team name"
        $ \testName -> name (newTeam testName) == testName

    -- Team 'users' test
    , testProperty "team users"
        $ \testName -> users (newTeam testName) == User.empty
    ]
