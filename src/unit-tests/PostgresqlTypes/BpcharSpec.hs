module PostgresqlTypes.BpcharSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified Data.Text as Text
import qualified PostgresqlTypes.Bpchar as Bpchar
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @(Bpchar.Bpchar 1))

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @(Bpchar.Bpchar 1))
    Scripts.testIsScalar (Proxy @(Bpchar.Bpchar 42))

  describe "Bpchar 10" do
    describe "Constructors" do
      describe "normalizeFromText" do
        it "truncates text exceeding max length" do
          let bpchar = Bpchar.normalizeFromText @10 "Hello World!"
          Text.length (Bpchar.toText bpchar) `shouldBe` 10

        it "pads text shorter than max length" do
          let bpchar = Bpchar.normalizeFromText @10 "Hello"
          Bpchar.toText bpchar `shouldBe` "Hello     "

        it "preserves characters including null" do
          let bpchar = Bpchar.normalizeFromText @10 "hel\NULlo"
          -- normalizeFromText doesn't strip null characters, it just truncates/pads
          -- The null character is preserved in the first 10 characters
          Text.length (Bpchar.toText bpchar) `shouldBe` 10

      describe "refineFromText" do
        it "rejects text exceeding max length" do
          Bpchar.refineFromText @10 "Hello World!" `shouldBe` Nothing

        it "rejects text with null characters" do
          Bpchar.refineFromText @10 "hel\NULlo" `shouldBe` Nothing

        it "accepts valid text with exact length" do
          let result = Bpchar.refineFromText @10 "HelloWorld"
          result `shouldSatisfy` isJust
          fmap Bpchar.toText result `shouldBe` Just "HelloWorld"

    describe "Accessors" do
      describe "toText" do
        it "extracts text value (padded)" do
          let bpchar = Bpchar.normalizeFromText @10 "test"
          Bpchar.toText bpchar `shouldBe` "test      "

    describe "Property Tests" do
      it "normalizeFromText is idempotent" do
        property \(t :: Text.Text) ->
          let normalized1 = Bpchar.normalizeFromText @10 t
              normalized2 = Bpchar.normalizeFromText @10 (Bpchar.toText normalized1)
           in normalized1 === normalized2

      it "refineFromText succeeds for valid bpchars" do
        property \(bpchar :: Bpchar.Bpchar 10) ->
          let t = Bpchar.toText bpchar
              refined = Bpchar.refineFromText @10 t
           in refined === Just bpchar
