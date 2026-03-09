module PostgresqlTypes.CitextSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified Data.Text as Text
import qualified PostgresqlTypes.Citext as Citext
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Citext.Citext)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Citext.Citext)

  describe "Constructors" do
    describe "normalizeFromText" do
      it "strips null characters" do
        let pgText = Citext.normalizeFromText "hello\NULworld"
        Citext.toText pgText `shouldBe` "helloworld"

      it "preserves text without null characters" do
        let pgText = Citext.normalizeFromText "hello world"
        Citext.toText pgText `shouldBe` "hello world"

      it "handles multiple null characters" do
        let pgText = Citext.normalizeFromText "\NULa\NULb\NULc\NUL"
        Citext.toText pgText `shouldBe` "abc"

      it "handles empty text" do
        let pgText = Citext.normalizeFromText ""
        Citext.toText pgText `shouldBe` ""

    describe "refineFromText" do
      it "rejects text with null characters" do
        Citext.refineFromText "hello\NULworld" `shouldBe` Nothing

      it "accepts text without null characters" do
        let result = Citext.refineFromText "hello world"
        result `shouldSatisfy` isJust
        fmap Citext.toText result `shouldBe` Just "hello world"

      it "accepts empty text" do
        let result = Citext.refineFromText ""
        result `shouldSatisfy` isJust

  describe "Accessors" do
    describe "toText" do
      it "extracts text value" do
        let pgText = Citext.normalizeFromText "test"
        Citext.toText pgText `shouldBe` "test"

  describe "Property Tests" do
    it "normalizeFromText is idempotent" do
      property \(t :: Text.Text) ->
        let normalized1 = Citext.normalizeFromText t
            normalized2 = Citext.normalizeFromText (Citext.toText normalized1)
         in normalized1 === normalized2

    describe "refineFromText" do
      it "succeeds for text without nulls" do
        property \(pgText :: Citext.Citext) ->
          let t = Citext.toText pgText
              refined = Citext.refineFromText t
           in refined === Just pgText

      it "rejects text with nulls" do
        property \(NonEmpty prefix, NonEmpty suffix) ->
          let textWithNull = Text.pack prefix <> "\NUL" <> Text.pack suffix
           in Citext.refineFromText textWithNull === Nothing
