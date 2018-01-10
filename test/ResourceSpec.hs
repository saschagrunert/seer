module ResourceSpec (
    resourceProps,
    resourceSpec,
) where

import Seer.Entity ((|+), (|-))
import Seer.Resource (empty, name, newResource, toResources)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
import Test.Tasty.SmallCheck ((==>), testProperty)

-- Resource.hs related tests
-- Unit tests
resourceSpec :: Spec
resourceSpec = parallel $ do
    it "should succeed to create a new Resource"
        $          newResource "testResource"
        `shouldBe` newResource "testResource"

    it "should succeed to 'show' a Resource"
        $          show [newResource "test"]
        `shouldBe` "[Resource {name = \"test\"}]"

    it "should succeed to 'show' Resources"
        $          show [empty]
        `shouldBe` "[Resources (fromList [])]"

    it "should succeed to create empty Resources" $ empty `shouldBe` empty

-- Property tests
resourceProps :: TestTree
resourceProps = testGroup
    "ResourceSpec.hs"
    [
    -- 'Resource ==' test
      testProperty "resource equal"
        $ \testName -> newResource testName == newResource testName

    -- 'Resource =/' test
    , testProperty "resource not equal"
        $ \t1 t2 -> t1 /= t2 ==> newResource t1 /= newResource t2

    -- Resource 'show' test
    , testProperty "resource show" $ \testName ->
        show (newResource testName)
            == "Resource {name = \""
            ++ testName
            ++ "\"}"

    -- Resource 'name' test
    , testProperty "resource name"
        $ \testName -> name (newResource testName) == testName

    -- 'toResources' test
    , testProperty "toResources"
        $ \testName -> toResources (newResource testName) /= empty

    -- '|+' test
    , testProperty "'|+' (add)" $ \testName ->
        empty |+ newResource testName == toResources (newResource testName)

    -- '|-' test
    , testProperty "'|-' (remove)" $ \testName ->
        toResources (newResource testName) |- newResource testName == empty
    ]
