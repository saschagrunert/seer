module UserSpec (
    userProps,
    userSpec,
) where

import Seer.User (newEmptyUser, User(..))
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)
import Test.Tasty (testGroup, TestTree)

-- User.hs related tests
-- Unit tests
userSpec :: Spec
userSpec = parallel $ do
    it "should succeed to create a new empty User"
        $          newEmptyUser "test"
        `shouldBe` User {name = "test", actions = []}

    it "should succeed to 'show' a User list"
        $          show [newEmptyUser "test"]
        `shouldBe` "[User {name = \"test\", actions = []}]"

-- Property tests
userProps :: TestTree
userProps = testGroup
    "UserSpec.hs"
    [
    -- User creation test
      testProperty "user creation" $ \testName ->
        newEmptyUser testName == User {name = testName, actions = []}

    -- User 'show' test
    , testProperty "user show" $ \testName ->
        show (newEmptyUser testName)
            == "User {name = \""
            ++ testName
            ++ "\", actions = []}"

    -- User =/ test
    , testProperty "user not equal"
        $ \t1 t2 -> t1 /= t2 ==> newEmptyUser t1 /= newEmptyUser t2

    -- User 'name' test
    , testProperty "user name"
        $ \testName -> name (newEmptyUser testName) == testName

    -- User 'actions' test
    , testProperty "user actions"
        $ \testName -> null . actions $ newEmptyUser testName
    ]
