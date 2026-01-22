module UnitTests.Scripts where

import qualified Data.Attoparsec.Text
import Data.Proxy
import Data.Tagged
import qualified Data.Text as Text
import Data.Typeable
import qualified PostgresqlTypes.Algebra
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import Prelude

-- | Test textual encoder/decoder roundtrip
testIsScalar ::
  forall a.
  ( QuickCheck.Arbitrary a,
    Show a,
    Eq a,
    PostgresqlTypes.Algebra.IsScalar a,
    Typeable a
  ) =>
  Proxy a ->
  Spec
testIsScalar _ =
  let typeName = untag (PostgresqlTypes.Algebra.typeName @a)
      binEnc = PostgresqlTypes.Algebra.binaryEncoder @a
      binDec = PostgresqlTypes.Algebra.binaryDecoder @a
      txtEnc = PostgresqlTypes.Algebra.textualEncoder @a
      txtDec = PostgresqlTypes.Algebra.textualDecoder @a
   in describe (show (typeOf (undefined :: a))) do
        describe (Text.unpack typeName) do
          describe "IsScalar" do
            describe "Encoding via textualEncoder" do
              describe "And decoding via textualDecoder" do
                it "Should produce the original value" $
                  QuickCheck.property \(value :: a) ->
                    let encoded = TextBuilder.toText (txtEnc value)
                        decoding = Data.Attoparsec.Text.parseOnly txtDec encoded
                     in counterexample ("Encoded: " ++ show encoded) $
                          decoding === Right value

            describe "Encoding via binaryEncoder" do
              describe "And decoding via binaryDecoder" do
                it "Should produce the original value" $
                  QuickCheck.property \(value :: a) ->
                    let encoded = PtrPoker.Write.toByteString (binEnc value)
                        decoding = PtrPeeker.runVariableOnByteString binDec encoded
                     in decoding === Right (Right value)
