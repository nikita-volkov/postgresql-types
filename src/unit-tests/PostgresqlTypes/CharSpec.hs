module PostgresqlTypes.CharSpec (spec) where

import Data.Maybe
import Data.Word
import qualified PostgresqlTypes.Char as PgChar
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (Char)

spec :: Spec
spec = do
  describe "Char" do
    describe "Constructors" do
      describe "normalizeFromWord8" do
        it "creates Char from ASCII byte" do
          let pgChar = PgChar.normalizeFromWord8 65 -- 'A'
          PgChar.toWord8 pgChar `shouldBe` 65

        it "clears bit 7 for values above 127" do
          let pgChar = PgChar.normalizeFromWord8 200 -- Binary: 11001000
          PgChar.toWord8 pgChar `shouldBe` 72 -- Binary: 01001000 (bit 7 cleared)
      describe "refineFromWord8" do
        it "accepts ASCII values (0-127)" do
          let result = PgChar.refineFromWord8 65
          result `shouldSatisfy` isJust
          fmap PgChar.toWord8 result `shouldBe` Just 65

        it "rejects values above 127" do
          PgChar.refineFromWord8 128 `shouldBe` Nothing
          PgChar.refineFromWord8 255 `shouldBe` Nothing

      describe "normalizeFromChar" do
        it "creates Char from Haskell Char" do
          let pgChar = PgChar.normalizeFromChar 'A'
          PgChar.toChar pgChar `shouldBe` 'A'

      describe "refineFromChar" do
        it "accepts ASCII characters" do
          let result = PgChar.refineFromChar 'Z'
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toWord8" do
        it "extracts Word8 value" do
          let pgChar = PgChar.normalizeFromWord8 42
          PgChar.toWord8 pgChar `shouldBe` 42

      describe "toChar" do
        it "extracts Haskell Char" do
          let pgChar = PgChar.normalizeFromChar 'B'
          PgChar.toChar pgChar `shouldBe` 'B'

    describe "Property Tests" do
      it "roundtrips through toWord8 and normalizeFromWord8 for valid ASCII" do
        property \(w :: Word8) ->
          w <= 127 ==>
            let pgChar = PgChar.normalizeFromWord8 w
             in PgChar.toWord8 pgChar === w

      it "roundtrips through normalizeFromWord8 and toWord8" do
        property \(pgChar :: PgChar.Char) ->
          let w = PgChar.toWord8 pgChar
              pgChar' = PgChar.normalizeFromWord8 w
           in pgChar' === pgChar
