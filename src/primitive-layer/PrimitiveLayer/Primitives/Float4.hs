module PrimitiveLayer.Primitives.Float4 (Float4 (..)) where

import Data.Bits
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Float4 = Float4 Float
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Float4)

instance Primitive Float4 where
  typeName = Tagged "float4"
  baseOid = Tagged 700
  arrayOid = Tagged 1021
  binaryEncoder (Float4 x) = Write.bWord32 (castFloatToWord32 x)
  binaryDecoder = PeekyBlinders.statically (Right . Float4 . castWord32ToFloat <$> PeekyBlinders.beUnsignedInt4)
  textualEncoder (Float4 x) = TextBuilder.string (show x)
