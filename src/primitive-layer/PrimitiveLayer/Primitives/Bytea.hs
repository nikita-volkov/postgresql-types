module PrimitiveLayer.Primitives.Bytea (Bytea (..)) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Bytea = Bytea ByteString
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Bytea)

instance Primitive Bytea where
  typeName = Tagged "bytea"
  baseOid = Tagged 17
  arrayOid = Tagged 1001
  binaryEncoder (Bytea bs) =
    Write.byteString bs
  binaryDecoder =
    Right . Bytea <$> PeekyBlinders.remainderAsByteString
  textualEncoder (Bytea bs) =
    "\\x" <> foldMap TextBuilder.hexadecimal (ByteString.unpack bs)
