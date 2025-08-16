module PrimitiveLayer.Primitives.Oid (Oid (..)) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Oid = Oid Word32
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Oid)

instance Primitive Oid where
  typeName = Tagged "oid"
  baseOid = Tagged 26
  arrayOid = Tagged 1028
  binaryEncoder (Oid x) = Write.bWord32 x
  binaryDecoder = PeekyBlinders.statically (Right . Oid <$> PeekyBlinders.beUnsignedInt4)
  textualEncoder (Oid x) = TextBuilder.decimal x
