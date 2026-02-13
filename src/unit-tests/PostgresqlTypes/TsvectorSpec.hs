module PostgresqlTypes.TsvectorSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified Data.Text as Text
import qualified PostgresqlTypes.Tsvector as Tsvector
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Tsvector.Tsvector)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Tsvector.Tsvector)

  describe "Constructors" do
    describe "fromLexemeList" do
      it "creates Tsvector from valid lexemes" do
        let result = Tsvector.fromLexemeList [("hello", [(1, Tsvector.WeightA)]), ("world", [(2, Tsvector.WeightB)])]
        result `shouldSatisfy` (/= Nothing)

      it "rejects empty lexeme tokens" do
        Tsvector.fromLexemeList [("", [(1, Tsvector.WeightA)])] `shouldBe` Nothing

      it "rejects lexeme tokens with null characters" do
        Tsvector.fromLexemeList [("hel\NULlo", [(1, Tsvector.WeightA)])] `shouldBe` Nothing

      it "creates Tsvector from empty list" do
        let result = Tsvector.fromLexemeList []
        result `shouldSatisfy` (/= Nothing)
        fmap Tsvector.toLexemeList result `shouldBe` Just []

      it "sorts lexemes alphabetically" do
        let Just tsvec = Tsvector.fromLexemeList [("banana", []), ("apple", [])]
            lexemes = map fst (Tsvector.toLexemeList tsvec)
        lexemes `shouldBe` ["apple", "banana"]

      it "deduplicates lexemes" do
        let Just tsvec = Tsvector.fromLexemeList [("hello", [(1, Tsvector.WeightA)]), ("hello", [(2, Tsvector.WeightB)])]
            lexemes = map fst (Tsvector.toLexemeList tsvec)
        length lexemes `shouldBe` 1

    describe "normalizeFromLexemeList" do
      it "strips null characters from tokens" do
        let tsvec = Tsvector.normalizeFromLexemeList [("hel\NULlo", [(1, Tsvector.WeightA)])]
            lexemes = Tsvector.toLexemeList tsvec
        map fst lexemes `shouldBe` ["hello"]

      it "removes empty lexemes after stripping" do
        let tsvec = Tsvector.normalizeFromLexemeList [("\NUL", [(1, Tsvector.WeightA)])]
            lexemes = Tsvector.toLexemeList tsvec
        lexemes `shouldBe` []

      it "handles empty input" do
        let tsvec = Tsvector.normalizeFromLexemeList []
        Tsvector.toLexemeList tsvec `shouldBe` []

  describe "Accessors" do
    describe "toLexemeList" do
      it "extracts lexemes with positions" do
        let Just tsvec = Tsvector.fromLexemeList [("test", [(1, Tsvector.WeightA), (2, Tsvector.WeightC)])]
            [(token, positions)] = Tsvector.toLexemeList tsvec
        token `shouldBe` "test"
        length positions `shouldBe` 2

  describe "Weight" do
    it "has correct ordering" do
      Tsvector.WeightA `shouldSatisfy` (< Tsvector.WeightB)
      Tsvector.WeightB `shouldSatisfy` (< Tsvector.WeightC)
      Tsvector.WeightC `shouldSatisfy` (< Tsvector.WeightD)

    it "has correct Enum instances" do
      [Tsvector.WeightA ..] `shouldBe` [Tsvector.WeightA, Tsvector.WeightB, Tsvector.WeightC, Tsvector.WeightD]

  describe "Textual encoding" do
    it "encodes empty tsvector" do
      let Just tsvec = Tsvector.fromLexemeList []
       in show tsvec `shouldBe` "\"\""

    it "encodes lexeme without positions" do
      let Just tsvec = Tsvector.fromLexemeList [("hello", [])]
       in show tsvec `shouldBe` "\"'hello'\""

    it "encodes lexeme with single weighted position" do
      let Just tsvec = Tsvector.fromLexemeList [("hello", [(1, Tsvector.WeightA)])]
       in show tsvec `shouldBe` "\"'hello':1A\""

    it "encodes lexeme with default weight (D) by omitting it" do
      let Just tsvec = Tsvector.fromLexemeList [("hello", [(1, Tsvector.WeightD)])]
       in show tsvec `shouldBe` "\"'hello':1\""

    it "encodes multiple lexemes" do
      let Just tsvec = Tsvector.fromLexemeList [("apple", [(1, Tsvector.WeightA)]), ("banana", [(2, Tsvector.WeightB)])]
       in show tsvec `shouldBe` "\"'apple':1A 'banana':2B\""

    it "escapes single quotes in lexemes" do
      let Just tsvec = Tsvector.fromLexemeList [("it's", [(1, Tsvector.WeightA)])]
       in show tsvec `shouldBe` "\"'it''s':1A\""

    it "escapes backslashes in lexemes" do
      let Just tsvec = Tsvector.fromLexemeList [("back\\slash", [(1, Tsvector.WeightA)])]
       in show tsvec `shouldBe` "\"'back\\\\\\\\slash':1A\""

  describe "Textual decoding" do
    it "decodes empty string" do

      Tsvector.toLexemeList (read "\"\"") `shouldBe` []

    it "decodes lexeme without positions" do
      let tsvec = read "\"'hello'\"" :: Tsvector.Tsvector
          [(token, positions)] = Tsvector.toLexemeList tsvec
      token `shouldBe` "hello"
      positions `shouldBe` []

    it "decodes lexeme with weighted position" do
      let tsvec = read "\"'hello':1A\"" :: Tsvector.Tsvector
          [(token, positions)] = Tsvector.toLexemeList tsvec
      token `shouldBe` "hello"
      positions `shouldBe` [(1, Tsvector.WeightA)]

    it "decodes lexeme with default weight" do
      let tsvec = read "\"'hello':1\"" :: Tsvector.Tsvector
          [(_, positions)] = Tsvector.toLexemeList tsvec
      positions `shouldBe` [(1, Tsvector.WeightD)]

    it "decodes multiple positions" do
      let tsvec = read "\"'hello':1A,2B,3C\"" :: Tsvector.Tsvector
          [(_, positions)] = Tsvector.toLexemeList tsvec
      length positions `shouldBe` 3

    it "decodes escaped single quotes" do
      let tsvec = read "\"'it''s':1A\"" :: Tsvector.Tsvector
          [(token, _)] = Tsvector.toLexemeList tsvec
      token `shouldBe` "it's"

    it "decodes escaped backslashes" do
      let tsvec = read "\"'back\\\\\\\\slash':1A\"" :: Tsvector.Tsvector
          [(token, _)] = Tsvector.toLexemeList tsvec
      token `shouldBe` "back\\slash"

  describe "Property Tests" do
    it "roundtrips through toLexemeList and fromLexemeList" do
      property \(tsvec :: Tsvector.Tsvector) ->
        let lexemes = Tsvector.toLexemeList tsvec
            reconstructed = Tsvector.fromLexemeList lexemes
         in reconstructed === Just tsvec

    it "fromLexemeList is idempotent via normalization" do
      property \(tsvec :: Tsvector.Tsvector) ->
        let lexemes = Tsvector.toLexemeList tsvec
            Just tsvec' = Tsvector.fromLexemeList lexemes
            lexemes' = Tsvector.toLexemeList tsvec'
            Just tsvec'' = Tsvector.fromLexemeList lexemes'
         in tsvec' === tsvec''

    it "normalizeFromLexemeList produces same result as fromLexemeList for valid input" do
      property \(tsvec :: Tsvector.Tsvector) ->
        let lexemes = Tsvector.toLexemeList tsvec
            normalized = Tsvector.normalizeFromLexemeList lexemes
            Just refined = Tsvector.fromLexemeList lexemes
         in normalized === refined
