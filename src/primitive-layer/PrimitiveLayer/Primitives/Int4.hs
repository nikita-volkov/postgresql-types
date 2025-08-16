module PrimitiveLayer.Primitives.Int4 (Int4 (..)) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Int4 = Int4 Int32
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Int4)

instance Primitive Int4 where
  typeName = Tagged "int4"
  baseOid = Tagged 23
  arrayOid = Tagged 1007
  binaryEncoder (Int4 x) = Write.bInt32 x
  binaryDecoder = PeekyBlinders.statically (Right . Int4 <$> PeekyBlinders.beSignedInt4)
  textualEncoder (Int4 x) = TextBuilder.decimal x
