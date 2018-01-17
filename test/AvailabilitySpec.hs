module AvailabilitySpec (
    availabilityProps,
    availabilitySpec,
) where

import Data.ByteString.Char8 (pack)
import Data.Yaml (decode, encode)
import Seer.Availability (available, availableFromTo, notAvailable, notAvailableFromTo, reserve, weeklyNotAvailable)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe)
{- import Test.Tasty.SmallCheck ((==>), testProperty) -}

-- Availability.hs related tests
-- Unit tests
availabilitySpec :: Spec
availabilitySpec = parallel $ do
    it "should succeed to availability"
        $          return available
        `shouldBe` availableFromTo "00:00" "23:59"

    it "should succeed to create non availability"
        $          return notAvailable
        `shouldBe` notAvailableFromTo "00:00" "23:59"

    it "should fail to reserve time in non availability"
        $          reserve "05:30" "06:30" notAvailable
        `shouldBe` Nothing

    it "should succeed to reserve all time"
        $          (availableFromTo "06:23" "07:12" >>= reserve "06:23" "07:12")
        `shouldBe` Just notAvailable

    it "should succeed to reserve multiple times"
        $          (   availableFromTo "15:55" "17:45"
                   >>= reserve         "15:55" "16:23"
                   >>= reserve         "16:24" "17:45"
                   )
        `shouldBe` Just notAvailable

    it "should fail to reserve above time boundaries"
        $          (availableFromTo "10:00" "11:00" >>= reserve "09:59" "11:01")
        `shouldBe` Nothing

    it "should fail to create availability with wrong timespan"
        $          availableFromTo "12:12" "09:09"
        `shouldBe` Nothing

    it "should fail to create availability with wrong time"
        $          availableFromTo "2:12" "09:09"
        `shouldBe` Nothing

    it "should succeed to encode availability as YAML"
        $          encode (availableFromTo "02:12" "09:09")
        `shouldBe` pack "- - 02:12:00\n  - 09:09:00\n"

    it "should succeed to encode non availability YAML"
        $          encode notAvailable
        `shouldBe` pack "[]\n"

    it "should succeed to encode full availability YAML"
        $          encode available
        `shouldBe` pack "- - 00:00:00\n  - 23:59:00\n"

    it "should succeed to decode availability as YAML"
        $          availableFromTo "02:12" "09:09"
        `shouldBe` decode (pack "- - 02:12:00\n  - 09:09:00\n")

    it "should succeed to decode non availability YAML"
        $          return notAvailable
        `shouldBe` decode (pack "[]\n")

    it "should succeed to decode full availability YAML"
        $          return available
        `shouldBe` decode (pack "- - 00:00:00\n  - 23:59:00\n")

-- Property tests
availabilityProps :: TestTree
availabilityProps = testGroup "TimeSpec.hs" []
