-- | The DateParser tests
--
-- @since 0.1.0

module DateParserSpec
  ( dateParserSpec
  ) where

import Data.Either        (isLeft)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock    (UTCTime (UTCTime))
import Seer.DateParser    (parseDate)
import Test.Tasty.Hspec   (Spec
                          ,it
                          ,parallel
                          ,shouldBe)
import TestData           (testTime)

-- DateParser.hs related tests
-- Unit tests
dateParserSpec :: Spec
dateParserSpec = parallel $ do
  it "schould succeed to parse an absolute EU date" $ do
    let res = parseDate testTime "01.02.2018"
    res `shouldBe` Right (UTCTime (fromGregorian 2018 2 1) 0)

  it "schould succeed to parse an absolute EU date without year" $ do
    let res = parseDate testTime "01.02"
    res `shouldBe` Right (UTCTime (fromGregorian 0 2 1) 0)

  it "schould succeed to parse an absolute EU date with short year" $ do
    let res = parseDate testTime "4.4.18"
    res `shouldBe` Right (UTCTime (fromGregorian 2018 4 4) 0)

  it "schould succeed to parse an absolute EU date with time" $ do
    let res = parseDate testTime "5.2.2000 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 2 5) 59520)

  it "schould succeed to parse an absolute EU date with time (comma)" $ do
    let res = parseDate testTime "5.2.2000, 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 2 5) 59520)

  it "schould succeed to parse an absolute EU short date with time" $ do
    let res = parseDate testTime "6.12.00 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 12 6) 59520)

  it "schould succeed to parse an absolute EU date with 12hour time" $ do
    let res = parseDate testTime "6.12.00 11:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 12 6) 39600)

  it "schould succeed to parse an absolute EU date without year with time" $ do
    let res = parseDate testTime "5.2 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 0 2 5) 59520)

  it
      "schould succeed to parse an absolute EU date without year with time (comma)"
    $ do
        let res = parseDate testTime "5.2, 16:32"
        res `shouldBe` Right (UTCTime (fromGregorian 0 2 5) 59520)

  it "schould succeed to parse an absolute EU short date without year with time"
    $ do
        let res = parseDate testTime "6.12 16:32"
        res `shouldBe` Right (UTCTime (fromGregorian 0 12 6) 59520)

  it
      "schould succeed to parse an absolute EU date without year with 12hour time"
    $ do
        let res = parseDate testTime "6.12 11:00am"
        res `shouldBe` Right (UTCTime (fromGregorian 0 12 6) 39600)

  it "schould succeed to parse an absolute US date" $ do
    let res = parseDate testTime "2018/02/01"
    res `shouldBe` Right (UTCTime (fromGregorian 2018 2 1) 0)

  it "schould succeed to parse an absolute US date without year" $ do
    let res = parseDate testTime "02/01"
    res `shouldBe` Right (UTCTime (fromGregorian 0 2 1) 0)

  it "schould succeed to parse an absolute US date with short year" $ do
    let res = parseDate testTime "18/4/4"
    res `shouldBe` Right (UTCTime (fromGregorian 2018 4 4) 0)

  it "schould succeed to parse an absolute US date with time" $ do
    let res = parseDate testTime "2000/2/5 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 2 5) 59520)

  it "schould succeed to parse an absolute US date with time (comma)" $ do
    let res = parseDate testTime "2000/2/5, 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 2 5) 59520)

  it "schould succeed to parse an absolute US short date with time" $ do
    let res = parseDate testTime "00/12/6 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 12 6) 59520)

  it "schould succeed to parse an absolute US date with 12hour time" $ do
    let res = parseDate testTime "00/12/6 11:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 12 6) 39600)

  it "schould succeed to parse an absolute US date without year with time" $ do
    let res = parseDate testTime "2/5 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 0 2 5) 59520)

  it
      "schould succeed to parse an absolute US date without year with time (comma)"
    $ do
        let res = parseDate testTime "2/5, 16:32"
        res `shouldBe` Right (UTCTime (fromGregorian 0 2 5) 59520)

  it "schould succeed to parse an absolute US short date without year with time"
    $ do
        let res = parseDate testTime "12/6 16:32"
        res `shouldBe` Right (UTCTime (fromGregorian 0 12 6) 59520)

  it
      "schould succeed to parse an absolute US date without year with 12hour time"
    $ do
        let res = parseDate testTime "12/6 11:00am"
        res `shouldBe` Right (UTCTime (fromGregorian 0 12 6) 39600)

  it "schould succeed to parse an absolute STR date" $ do
    let res = parseDate testTime "01. February 2018"
    res `shouldBe` Right (UTCTime (fromGregorian 2018 2 1) 0)

  it "schould succeed to parse an absolute STR date without year" $ do
    let res = parseDate testTime "01. feb"
    res `shouldBe` Right (UTCTime (fromGregorian 0 2 1) 0)

  it "schould succeed to parse an absolute STR date with short year" $ do
    let res = parseDate testTime "4 april 18"
    res `shouldBe` Right (UTCTime (fromGregorian 2018 4 4) 0)

  it "schould succeed to parse an absolute STR date with time" $ do
    let res = parseDate testTime "5 FEB 2000 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 2 5) 59520)

  it "schould succeed to parse an absolute STR date with time (comma)" $ do
    let res = parseDate testTime "5. febru 2000, 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 2 5) 59520)

  it "schould succeed to parse an absolute STR short date with time" $ do
    let res = parseDate testTime "6. december 00 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 12 6) 59520)

  it "schould succeed to parse an absolute STR date with 12hour time" $ do
    let res = parseDate testTime "6 dec 00 11:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 2000 12 6) 39600)

  it "schould succeed to parse an absolute STR date without year with time" $ do
    let res = parseDate testTime "5. FebruarY 16:32"
    res `shouldBe` Right (UTCTime (fromGregorian 0 2 5) 59520)

  it
      "schould succeed to parse an absolute STR date without year with time (comma)"
    $ do
        let res = parseDate testTime "5. feb, 16:32"
        res `shouldBe` Right (UTCTime (fromGregorian 0 2 5) 59520)

  it
      "schould succeed to parse an absolute STR short date without year with time"
    $ do
        let res = parseDate testTime "6. dec 16:32"
        res `shouldBe` Right (UTCTime (fromGregorian 0 12 6) 59520)

  it
      "schould succeed to parse an absolute STR date without year with 12hour time"
    $ do
        let res = parseDate testTime "6 dec 11:00am"
        res `shouldBe` Right (UTCTime (fromGregorian 0 12 6) 39600)

  it "schould fail to parse an absolute EU date with wrong day" $ do
    let res = parseDate testTime "33.02.2018"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute EU date with wrong month" $ do
    let res = parseDate testTime "01.13.2018"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute EU date with wrong time" $ do
    let res = parseDate testTime "5.2.2000 99:32"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute US date with wrong day" $ do
    let res = parseDate testTime "2018/33/02"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute US date with wrong month" $ do
    let res = parseDate testTime "2018/3/33"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute US date with wrong time" $ do
    let res = parseDate testTime "2018/33/02 99:02"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute STR date with wrong day" $ do
    let res = parseDate testTime "33. sep 2019"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute STR date with wrong month" $ do
    let res = parseDate testTime "12. month"
    isLeft res `shouldBe` True

  it "schould fail to parse an absolute STR date with wrong time" $ do
    let res = parseDate testTime "13. july 99:02"
    isLeft res `shouldBe` True

  it "schould succeed to parse the last week" $ do
    let res = parseDate testTime "last week"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 27) 0)

  it "schould succeed to parse the last weekday (short)" $ do
    let res = parseDate testTime "last tue"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 28) 0)

  it "schould succeed to parse the last weekday (long)" $ do
    let res = parseDate testTime "last tuesday"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 28) 0)

  it "schould succeed to parse the last weekday (long) with time" $ do
    let res = parseDate testTime "last tuesday, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 28) 60)

  it "schould fail to parse the last weekday if weekday wrong" $ do
    let res = parseDate testTime "last xxx"
    isLeft res `shouldBe` True

  it "schould succeed to parse the last month" $ do
    let res = parseDate testTime "last month"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 27) 0)

  it "schould succeed to parse the last year" $ do
    let res = parseDate testTime "last year"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 1) 0)

  it "schould succeed to parse the last year" $ do
    let res = parseDate testTime "last test"
    isLeft res `shouldBe` True

  it "schould succeed to parse the last week with time" $ do
    let res = parseDate testTime "last week, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 27) 60)

  it "schould succeed to parse the last month with time" $ do
    let res = parseDate testTime "last month, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 27) 60)

  it "schould succeed to parse the last year with time" $ do
    let res = parseDate testTime "last year, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 1) 60)

  it "schould succeed to parse the next week" $ do
    let res = parseDate testTime "next week"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 0)

  it "schould fail to parse the last weekday if weekday wrong" $ do
    let res = parseDate testTime "next xxx"
    isLeft res `shouldBe` True

  it "schould succeed to parse the next weekday (short)" $ do
    let res = parseDate testTime "next wed"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 5) 0)

  it "schould succeed to parse the next weekday (long)" $ do
    let res = parseDate testTime "next wednesday"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 5) 0)

  it "schould succeed to parse the next weekday (long) with time" $ do
    let res = parseDate testTime "next wednesday, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 5) 60)

  it "schould succeed to parse the next month" $ do
    let res = parseDate testTime "next month"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 0)

  it "schould succeed to parse the next year" $ do
    let res = parseDate testTime "next year"
    res `shouldBe` Right (UTCTime (fromGregorian 1 1 1) 0)

  it "schould succeed to parse the next week with time" $ do
    let res = parseDate testTime "next week, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 60)

  it "schould succeed to parse the next month with time" $ do
    let res = parseDate testTime "next month, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 60)

  it "schould succeed to parse the next year with time" $ do
    let res = parseDate testTime "next year, 00:01"
    res `shouldBe` Right (UTCTime (fromGregorian 1 1 1) 60)

  it "schould succeed to parse the next year" $ do
    let res = parseDate testTime "next test"
    isLeft res `shouldBe` True

  it "schould succeed to parse 'in days'" $ do
    let res = parseDate testTime "in 2 days"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 0)

  it "schould succeed to parse 'in days' with 24h time" $ do
    let res = parseDate testTime "in 2 days, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 43200)

  it "schould succeed to parse 'in days' with 12h time" $ do
    let res = parseDate testTime "in 2 days, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 3) 43200)

  it "schould succeed to parse 'in months'" $ do
    let res = parseDate testTime "in 2 months"
    res `shouldBe` Right (UTCTime (fromGregorian 0 3 1) 0)

  it "schould succeed to parse 'in months' with 24h time" $ do
    let res = parseDate testTime "in 2 months, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian 0 3 1) 43200)

  it "schould succeed to parse 'in months' with 12h time" $ do
    let res = parseDate testTime "in 2 months, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 0 3 1) 43200)

  it "schould succeed to parse 'in years'" $ do
    let res = parseDate testTime "in 2 years"
    res `shouldBe` Right (UTCTime (fromGregorian 2 1 1) 0)

  it "schould succeed to parse 'in years' with 24h time" $ do
    let res = parseDate testTime "in 2 years, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian 2 1 1) 43200)

  it "schould succeed to parse 'in years' with 12h time" $ do
    let res = parseDate testTime "in 2 years, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 2 1 1) 43200)

  it "schould fail to parse 'in test'" $ do
    let res = parseDate testTime "in 2 test"
    isLeft res `shouldBe` True

  it "schould fail to parse 'in x days" $ do
    let res = parseDate testTime "in x days"
    isLeft res `shouldBe` True

  it "schould succeed to parse 'days ago'" $ do
    let res = parseDate testTime "2 days ago"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 30) 0)

  it "schould succeed to parse 'days ago' with 24h time" $ do
    let res = parseDate testTime "2 days ago, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 30) 43200)

  it "schould succeed to parse 'days ago' with 12h time" $ do
    let res = parseDate testTime "2 days ago, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 30) 43200)

  it "schould succeed to parse 'months ago'" $ do
    let res = parseDate testTime "2 months ago"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 11 1) 0)

  it "schould succeed to parse 'months ago' with 24h time" $ do
    let res = parseDate testTime "2 months ago, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 11 1) 43200)

  it "schould succeed to parse 'months ago' with 12h time" $ do
    let res = parseDate testTime "2 months ago, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 11 1) 43200)

  it "schould succeed to parse 'years ago'" $ do
    let res = parseDate testTime "2 years ago"
    res `shouldBe` Right (UTCTime (fromGregorian (-2) 1 1) 0)

  it "schould succeed to parse 'years ago' with 24h time" $ do
    let res = parseDate testTime "2 years ago, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian (-2) 1 1) 43200)

  it "schould succeed to parse 'years ago' with 12h time" $ do
    let res = parseDate testTime "2 years ago, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian (-2) 1 1) 43200)

  it "schould fail to parse '2 test'" $ do
    let res = parseDate testTime "2 test"
    isLeft res `shouldBe` True

  it "schould fail to parse 'x days ago'" $ do
    let res = parseDate testTime "x days ago"
    isLeft res `shouldBe` True

  it "schould succed to parse today" $ do
    let res = parseDate testTime "today"
    res `shouldBe` Right (UTCTime (fromGregorian 0 0 0) 0)

  it "schould succed to parse today with time" $ do
    let res = parseDate testTime "today, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian 0 0 0) 43200)

  it "schould fail to parse today with wrong time" $ do
    let res = parseDate testTime "today, 99:00"
    isLeft res `shouldBe` True

  it "schould succed to parse now" $ do
    let res = parseDate testTime "now"
    res `shouldBe` Right (UTCTime (fromGregorian 0 0 0) 0)

  it "schould succed to parse now with 12 hour time" $ do
    let res = parseDate testTime "now, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 0 0 0) 0)

  it "schould succed to parse now with 24 hour time" $ do
    let res = parseDate testTime "now, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian 0 0 0) 0)

  it "schould succed to parse tomorrow" $ do
    let res = parseDate testTime "tomorrow"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 2) 0)

  it "schould succed to parse tomorrow with 12 hour time" $ do
    let res = parseDate testTime "tomorrow, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 2) 43200)

  it "schould succed to parse tomorrow with 24 hour time" $ do
    let res = parseDate testTime "tomorrow, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian 0 1 2) 43200)

  it "schould fail to parse tomorrow with wrong time" $ do
    let res = parseDate testTime "tomorrow, 99:00"
    isLeft res `shouldBe` True

  it "schould succed to parse yesterday" $ do
    let res = parseDate testTime "yesterday"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 31) 0)

  it "schould succed to parse yesterday with 12 hour time" $ do
    let res = parseDate testTime "yesterday, 12:00am"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 31) 43200)

  it "schould succed to parse yesterday with 24 hour time" $ do
    let res = parseDate testTime "yesterday, 12:00"
    res `shouldBe` Right (UTCTime (fromGregorian (-1) 12 31) 43200)

  it "schould fail to parse yesterday with wrong time" $ do
    let res = parseDate testTime "yesterday, 99:00"
    isLeft res `shouldBe` True
