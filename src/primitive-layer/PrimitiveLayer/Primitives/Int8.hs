module PrimitiveLayer.Primitives.Int8 (Int8 (..)) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Int8)
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Int8 = Int8 Int64
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Int8)

instance Primitive Int8 where
  typeName = Tagged "int8"
  baseOid = Tagged 20
  arrayOid = Tagged 1016
  binaryEncoder (Int8 x) = Write.bInt64 x
  binaryDecoder = PeekyBlinders.statically (Right . Int8 <$> PeekyBlinders.beSignedInt8)
  textualEncoder (Int8 x) = TextBuilder.decimal x
