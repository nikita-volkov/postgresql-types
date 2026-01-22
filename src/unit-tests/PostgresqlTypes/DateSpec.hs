module PostgresqlTypes.DateSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified Data.Time as Time
import qualified PostgresqlTypes.Date as Date
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Date.Date)

  describe "Constructors" do
    describe "normalizeFromDay" do
      it "clamps dates below minimum" do
        let tooEarly = Time.fromGregorian (-5000) 1 1
            date = Date.normalizeFromDay tooEarly
        date `shouldBe` minBound

      it "clamps dates above maximum" do
        let tooLate = Time.fromGregorian 6000000 12 31
            date = Date.normalizeFromDay tooLate
        date `shouldBe` maxBound

      it "accepts dates within range" do
        let day = Time.fromGregorian 2023 6 15
            date = Date.normalizeFromDay day
        Date.toDay date `shouldBe` day

      it "accepts minimum date" do
        let minDay = Time.fromGregorian (-4712) 1 1
            date = Date.normalizeFromDay minDay
        Date.toDay date `shouldBe` minDay

    describe "refineFromDay" do
      it "rejects dates below minimum" do
        let tooEarly = Time.fromGregorian (-5000) 1 1
        Date.refineFromDay tooEarly `shouldBe` Nothing

      it "rejects dates above maximum" do
        let tooLate = Time.fromGregorian 6000000 12 31
        Date.refineFromDay tooLate `shouldBe` Nothing

      it "accepts dates within range" do
        let day = Time.fromGregorian 2023 6 15
            result = Date.refineFromDay day
        result `shouldSatisfy` isJust
        fmap Date.toDay result `shouldBe` Just day

      it "accepts minimum date" do
        let minDay = Time.fromGregorian (-4712) 1 1
            result = Date.refineFromDay minDay
        result `shouldSatisfy` isJust

      it "accepts maximum date" do
        let maxDay = Time.fromGregorian 5874897 12 31
            result = Date.refineFromDay maxDay
        result `shouldSatisfy` isJust

  describe "Accessors" do
    describe "toDay" do
      it "extracts Day value" do
        let day = Time.fromGregorian 2023 6 15
            date = Date.normalizeFromDay day
        Date.toDay date `shouldBe` day

  describe "Property Tests" do
    it "normalizeFromDay is idempotent" do
      property \(day :: Time.Day) ->
        let normalized1 = Date.normalizeFromDay day
            normalized2 = Date.normalizeFromDay (Date.toDay normalized1)
         in normalized1 === normalized2

    it "refineFromDay succeeds for valid dates" do
      property \(date :: Date.Date) ->
        let day = Date.toDay date
            refined = Date.refineFromDay day
         in refined === Just date

    it "toDay . normalizeFromDay preserves valid dates" do
      property \(date :: Date.Date) ->
        let day = Date.toDay date
            restored = Date.normalizeFromDay day
         in restored === date
