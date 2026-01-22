module VarcharSpec (spec) where

import Data.Maybe
import Data.Proxy
import qualified Data.Text as Text
import qualified PostgresqlTypes.Varchar as Varchar
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Varchar" do
    describe "Varchar 10" do
      describe "Constructors" do
        describe "normalizeFromText" do
          it "truncates text exceeding max length" do
            let varchar = Varchar.normalizeFromText @10 "Hello World!"
            Text.length (Varchar.toText varchar) `shouldBe` 10

          it "preserves text within max length" do
            let varchar = Varchar.normalizeFromText @10 "Hello"
            Varchar.toText varchar `shouldBe` "Hello"

          it "strips null characters" do
            let varchar = Varchar.normalizeFromText @10 "hel\NULlo"
            Varchar.toText varchar `shouldBe` "hello"

        describe "refineFromText" do
          it "rejects text exceeding max length" do
            Varchar.refineFromText @10 "Hello World!" `shouldBe` Nothing

          it "rejects text with null characters" do
            Varchar.refineFromText @10 "hel\NULlo" `shouldBe` Nothing

          it "accepts valid text" do
            let result = Varchar.refineFromText @10 "Hello"
            result `shouldSatisfy` isJust
            fmap Varchar.toText result `shouldBe` Just "Hello"

      describe "Accessors" do
        describe "toText" do
          it "extracts text value" do
            let varchar = Varchar.normalizeFromText @10 "test"
            Varchar.toText varchar `shouldBe` "test"

      describe "Property Tests" do
        it "normalizeFromText is idempotent" do
          property \(t :: Text.Text) ->
            let normalized1 = Varchar.normalizeFromText @10 t
                normalized2 = Varchar.normalizeFromText @10 (Varchar.toText normalized1)
             in normalized1 === normalized2

        it "refineFromText succeeds for valid varchars" do
          property \(varchar :: Varchar.Varchar 10) ->
            let t = Varchar.toText varchar
                refined = Varchar.refineFromText @10 t
             in refined === Just varchar
