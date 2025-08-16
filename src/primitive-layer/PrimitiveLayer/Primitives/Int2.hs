module PrimitiveLayer.Primitives.Int2 (Int2 (..)) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Int2 = Int2 Int16
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Int2)

instance Primitive Int2 where
  typeName = Tagged "int2"
  baseOid = Tagged 21
  arrayOid = Tagged 1005
  binaryEncoder (Int2 x) = Write.bInt16 x
  binaryDecoder = PeekyBlinders.statically (Right . Int2 <$> PeekyBlinders.beSignedInt2)
  textualEncoder (Int2 x) = TextBuilder.decimal x
