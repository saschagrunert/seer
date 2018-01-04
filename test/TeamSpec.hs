module TeamSpec (
    teamProps,
    teamSpec,
) where

import Seer.Team (newEmptyTeam, Team(..))
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)
import Test.Tasty (testGroup, TestTree)

-- User.hs related tests
-- Unit tests
teamSpec :: Spec
teamSpec = parallel $ do
    it "should succeed to create a new empty Team"
        $          newEmptyTeam "test"
        `shouldBe` Team {name = "test", actions = [], users = []}

    it "should succeed to 'show' a Team list"
        $          show [newEmptyTeam "test"]
        `shouldBe` "[Team {name = \"test\", actions = [], users = []}]"

-- Property tests
teamProps :: TestTree
teamProps = testGroup
    "TeamSpec.hs"
    [
    -- Team creation test
      testProperty "team creation" $ \testName -> newEmptyTeam testName
        == Team {name = testName, actions = [], users = []}

    -- Team 'show' test
    , testProperty "team show" $ \testName ->
        show (newEmptyTeam testName)
            == "Team {name = \""
            ++ testName
            ++ "\", actions = [], users = []}"

    -- Team =/ test
    , testProperty "team not equal"
        $ \t1 t2 -> t1 /= t2 ==> newEmptyTeam t1 /= newEmptyTeam t2

    -- Team 'name' test
    , testProperty "team name"
        $ \testName -> name (newEmptyTeam testName) == testName

    -- Team 'actions' test
    , testProperty "team actions"
        $ \testName -> null . actions $ newEmptyTeam testName

    -- Team 'users' test
    , testProperty "team users"
        $ \testName -> null . users $ newEmptyTeam testName
    ]
