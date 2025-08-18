module Main (main) where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.Proxy
import qualified Data.Aeson as Aeson
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
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Bool @PrimitiveLayer.Bool Proxy Proxy)

  describe "Text" do
    describe "Data.Text.Text" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Text.Text @PrimitiveLayer.Text Proxy Proxy)

  describe "Bytea" do
    describe "ByteString" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @ByteString @PrimitiveLayer.Bytea Proxy Proxy)

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

  describe "Date" do
    describe "Data.Time.Day" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Day @PrimitiveLayer.Date Proxy Proxy)

  describe "Uuid" do
    describe "Data.UUID.UUID" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @UUID.UUID @PrimitiveLayer.Uuid Proxy Proxy)

  describe "Int2" do
    describe "Int16" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int16 @PrimitiveLayer.Int2 Proxy Proxy)

  describe "Int4" do
    describe "Int32" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int32 @PrimitiveLayer.Int4 Proxy Proxy)

  describe "Int8" do
    describe "Int64" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int64 @PrimitiveLayer.Int8 Proxy Proxy)

  describe "Interval" do
    describe "(Int32, Int32, Int64)" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @(Int32, Int32, Int64) @PrimitiveLayer.Interval Proxy Proxy)

  describe "Float4" do
    describe "Float" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Float @PrimitiveLayer.Float4 Proxy Proxy)

  describe "Float8" do
    describe "Double" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Double @PrimitiveLayer.Float8 Proxy Proxy)

  describe "Numeric" do
    describe "Data.Scientific.Scientific" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Scientific.Scientific @PrimitiveLayer.Numeric Proxy Proxy)

  describe "Money" do
    describe "Int64" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Int64 @PrimitiveLayer.Money Proxy Proxy)

  describe "Json" do
    describe "Data.Aeson.Value" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Aeson.Value @PrimitiveLayer.Json Proxy Proxy)

  describe "Oid" do
    describe "Word32" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @Word32 @PrimitiveLayer.Oid Proxy Proxy)

  describe "Timestamp" do
    describe "Data.Time.LocalTime" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @LocalTime @PrimitiveLayer.Timestamp Proxy Proxy)

  describe "Timestamptz" do
    describe "Data.Time.UTCTime" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @UTCTime @PrimitiveLayer.Timestamptz Proxy Proxy)

  describe "Time" do
    describe "Data.Time.TimeOfDay" do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties @TimeOfDay @PrimitiveLayer.Time Proxy Proxy)
