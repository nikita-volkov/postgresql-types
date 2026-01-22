module TextSpec (spec) where

import Data.Maybe
import qualified Data.Text as Text
import qualified PostgresqlTypes.Text as PgText
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Text" do
    describe "Constructors" do
      describe "normalizeFromText" do
        it "strips null characters" do
          let pgText = PgText.normalizeFromText "hello\NULworld"
          PgText.toText pgText `shouldBe` "helloworld"

        it "preserves text without null characters" do
          let pgText = PgText.normalizeFromText "hello world"
          PgText.toText pgText `shouldBe` "hello world"

        it "handles multiple null characters" do
          let pgText = PgText.normalizeFromText "\NULa\NULb\NULc\NUL"
          PgText.toText pgText `shouldBe` "abc"

        it "handles empty text" do
          let pgText = PgText.normalizeFromText ""
          PgText.toText pgText `shouldBe` ""

      describe "refineFromText" do
        it "rejects text with null characters" do
          PgText.refineFromText "hello\NULworld" `shouldBe` Nothing

        it "accepts text without null characters" do
          let result = PgText.refineFromText "hello world"
          result `shouldSatisfy` isJust
          fmap PgText.toText result `shouldBe` Just "hello world"

        it "accepts empty text" do
          let result = PgText.refineFromText ""
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toText" do
        it "extracts text value" do
          let pgText = PgText.normalizeFromText "test"
          PgText.toText pgText `shouldBe` "test"

    describe "Property Tests" do
      it "normalizeFromText is idempotent" do
        property \(t :: Text.Text) ->
          let normalized1 = PgText.normalizeFromText t
              normalized2 = PgText.normalizeFromText (PgText.toText normalized1)
           in normalized1 === normalized2

      it "refineFromText succeeds for text without nulls" do
        property \(pgText :: PgText.Text) ->
          let t = PgText.toText pgText
              refined = PgText.refineFromText t
           in refined === Just pgText

      it "refineFromText rejects text with nulls" do
        property \(NonEmpty prefix, NonEmpty suffix) ->
          let textWithNull = Text.pack prefix <> "\NUL" <> Text.pack suffix
           in PgText.refineFromText textWithNull === Nothing
