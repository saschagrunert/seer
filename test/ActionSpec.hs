module ActionSpec (
    actionProps,
    actionSpec,
) where

import Data.Maybe (isNothing)
import Seer.Action (empty, name, newAction, assigned)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)

-- Action.hs related tests
-- Unit tests
actionSpec :: Spec
actionSpec = parallel $ do
    it "should succeed to create a new empty Action"
        $          newAction "test"
        `shouldBe` newAction "test"

    it "should succeed to 'show' a Action"
        $          show [newAction "test"]
        `shouldBe` "[Action {name = \"test\", assigned = Nothing}]"

    it "should succeed to 'show' Actions"
        $          show [empty]
        `shouldBe` "[Actions []]"

    it "should succeed to create empty Actions" $ empty `shouldBe` empty

-- Property tests
actionProps :: TestTree
actionProps = testGroup
    "ActionSpec.hs"
    [
    -- 'Action ==' test
      testProperty "action equal"
        $ \testName -> newAction testName == newAction testName

    -- 'Action =/' test
    , testProperty "action not equal"
        $ \t1 t2 -> t1 /= t2 ==> newAction t1 /= newAction t2

    -- Action 'show' test
    , testProperty "action show" $ \testName ->
        show (newAction testName)
            == "Action {name = \""
            ++ testName
            ++ "\", assigned = Nothing}"

   -- Action 'name' test
    , testProperty "action name"
        $ \testName -> name (newAction testName) == testName

    -- Action 'assigned' test
    , testProperty "action users"
        $ \testName -> isNothing . assigned $ newAction testName
    ]
