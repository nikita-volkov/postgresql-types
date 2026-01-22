module UnitTests.Scripts where

import Control.Monad
import qualified Data.Attoparsec.Text
import Data.Proxy
import Data.Tagged
import qualified Data.Text as Text
import qualified PostgresqlTypes.Algebra
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck.Classes as Laws
import qualified TextBuilder
import Prelude

-- | Test textual encoder/decoder roundtrip
testIsScalar ::
  forall a.
  ( Arbitrary a,
    Show a,
    Read a,
    Eq a,
    PostgresqlTypes.Algebra.IsScalar a
  ) =>
  Proxy a ->
  Spec
testIsScalar proxy =
  let name = Text.unpack (untag (PostgresqlTypes.Algebra.typeSignature @a))
      binEnc = PostgresqlTypes.Algebra.binaryEncoder @a
      binDec = PostgresqlTypes.Algebra.binaryDecoder @a
      txtEnc = PostgresqlTypes.Algebra.textualEncoder @a
      txtDec = PostgresqlTypes.Algebra.textualDecoder @a
   in describe name do
        describe "Encoding via textualEncoder" do
          describe "And decoding via textualDecoder" do
            it "Should produce the original value" $
              property \(value :: a) ->
                let encoded = TextBuilder.toText (txtEnc value)
                    decoding = Data.Attoparsec.Text.parseOnly txtDec encoded
                 in counterexample ("Encoded: " ++ show encoded) $
                      decoding === Right value

        describe "Encoding via binaryEncoder" do
          describe "And decoding via binaryDecoder" do
            it "Should produce the original value" $
              property \(value :: a) ->
                let encoded = PtrPoker.Write.toByteString (binEnc value)
                    decoding = PtrPeeker.runVariableOnByteString binDec encoded
                 in decoding === Right (Right value)

        describe "Show/Read laws" do
          forM_ (Laws.lawsProperties (Laws.showReadLaws proxy)) \(name, property) ->
            it name property
