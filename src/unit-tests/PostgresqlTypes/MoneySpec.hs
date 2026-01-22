module PostgresqlTypes.MoneySpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Int
import qualified PostgresqlTypes.Money as Money
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Money.Money)

  describe "Constructors" do
    describe "fromInt64" do
      it "creates Money from positive Int64" do
        let pgMoney = Money.fromInt64 12345
        Money.toInt64 pgMoney `shouldBe` 12345

      it "creates Money from negative Int64" do
        let pgMoney = Money.fromInt64 (-12345)
        Money.toInt64 pgMoney `shouldBe` (-12345)

      it "creates Money from zero" do
        let pgMoney = Money.fromInt64 0
        Money.toInt64 pgMoney `shouldBe` 0

  describe "Accessors" do
    describe "toInt64" do
      it "extracts Int64 value representing cents" do
        let pgMoney = Money.fromInt64 9999
        Money.toInt64 pgMoney `shouldBe` 9999

  describe "Property Tests" do
    it "roundtrips through toInt64 and fromInt64" do
      property \(i :: Int64) ->
        let pgMoney = Money.fromInt64 i
         in Money.toInt64 pgMoney === i

    it "roundtrips through fromInt64 and toInt64" do
      property \(pgMoney :: Money.Money) ->
        let i = Money.toInt64 pgMoney
            pgMoney' = Money.fromInt64 i
         in pgMoney' === pgMoney
