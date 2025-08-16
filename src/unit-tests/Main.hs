module Main (main) where

import Data.Foldable
import Data.Proxy
import Data.Time
import Data.Word
import qualified LawfulConversions
import qualified PrimitiveLayer
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main = hspec do
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
