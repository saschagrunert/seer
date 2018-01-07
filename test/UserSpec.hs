module UserSpec (
    userProps,
    userSpec,
) where

import Seer.User (empty, name, newUser)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)

-- User.hs related tests
-- Unit tests
userSpec :: Spec
userSpec = parallel $ do
    it "should succeed to create a new User"
        $          newUser "testUser"
        `shouldBe` newUser "testUser"

    it "should succeed to 'show' a User"
        $          show [newUser "test"]
        `shouldBe` "[User {name = \"test\"}]"

    it "should succeed to 'show' Users"
        $          show [empty]
        `shouldBe` "[Users (fromList [])]"

    it "should succeed to create empty Users" $ empty `shouldBe` empty

-- Property tests
userProps :: TestTree
userProps = testGroup
    "UserSpec.hs"
    [
    -- 'User ==' test
      testProperty "user equal"
        $ \testName -> newUser testName == newUser testName

    -- 'User =/' test
    , testProperty "user not equal"
        $ \t1 t2 -> t1 /= t2 ==> newUser t1 /= newUser t2

    -- User 'show' test
    , testProperty "user show" $ \testName ->
        show (newUser testName) == "User {name = \"" ++ testName ++ "\"}"

    -- Team 'name' test
    , testProperty "user name"
        $ \testName -> name (newUser testName) == testName
    ]
