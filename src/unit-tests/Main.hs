module Main (main) where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.Proxy
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Data.Time
import qualified Data.UUID as UUID
import Data.Word
import qualified LawfulConversions
import qualified PrimitiveLayer
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main = hspec do
  describe "Bool" do
    describe "Data.Bool.Bool" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Bool @PrimitiveLayer.Bool Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Bool @PrimitiveLayer.Bool Proxy Proxy)

  describe "Text" do
    describe "Data.Text.Text" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Text.Text @PrimitiveLayer.Text Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Text.Text @PrimitiveLayer.Text Proxy Proxy)

  describe "Bytea" do
    describe "ByteString" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @ByteString @PrimitiveLayer.Bytea Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @ByteString @PrimitiveLayer.Bytea Proxy Proxy)

  describe "Date" do
    describe "Data.Time.Day" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Day @PrimitiveLayer.Date Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Day @PrimitiveLayer.Date Proxy Proxy)

  describe "Uuid" do
    describe "Data.UUID.UUID" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @UUID.UUID @PrimitiveLayer.Uuid Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @UUID.UUID @PrimitiveLayer.Uuid Proxy Proxy)

  describe "Int2" do
    describe "Int16" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Int16 @PrimitiveLayer.Int2 Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int16 @PrimitiveLayer.Int2 Proxy Proxy)

  describe "Int4" do
    describe "Int32" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Int32 @PrimitiveLayer.Int4 Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int32 @PrimitiveLayer.Int4 Proxy Proxy)

  describe "Int8" do
    describe "Int64" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Int64 @PrimitiveLayer.Int8 Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int64 @PrimitiveLayer.Int8 Proxy Proxy)

  describe "Float4" do
    describe "Float" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Float @PrimitiveLayer.Float4 Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Float @PrimitiveLayer.Float4 Proxy Proxy)

  describe "Float8" do
    describe "Double" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Double @PrimitiveLayer.Float8 Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Double @PrimitiveLayer.Float8 Proxy Proxy)

  describe "Numeric" do
    describe "Data.Scientific.Scientific" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Scientific.Scientific @PrimitiveLayer.Numeric Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Scientific.Scientific @PrimitiveLayer.Numeric Proxy Proxy)

  describe "Oid" do
    describe "Word32" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @Word32 @PrimitiveLayer.Oid Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Word32 @PrimitiveLayer.Oid Proxy Proxy)

  describe "Timestamp" do
    describe "Data.Time.LocalTime" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @LocalTime @PrimitiveLayer.Timestamp Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @LocalTime @PrimitiveLayer.Timestamp Proxy Proxy)

  describe "Timestamptz" do
    describe "Data.Time.UTCTime" do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties @UTCTime @PrimitiveLayer.Timestamptz Proxy Proxy)
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @UTCTime @PrimitiveLayer.Timestamptz Proxy Proxy)

  -- Note: Interval ↔ DiffTime conversion is intentionally omitted as it would not be lawful
  -- due to the structural differences between these types.

  -- Keep existing tests from previous implementation
  describe "Char" do
    describe "Word8" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Word8 @PrimitiveLayer.Char Proxy Proxy)
    describe "Data.Char.Char" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Char @PrimitiveLayer.Char Proxy Proxy)

  describe "Time" do
    describe "Data.Time.TimeOfDay" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @TimeOfDay @PrimitiveLayer.Time Proxy Proxy)
