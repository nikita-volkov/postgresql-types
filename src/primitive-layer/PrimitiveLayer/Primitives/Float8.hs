module PrimitiveLayer.Primitives.Float8 (Float8 (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Float8 = Float8 Double
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Float8)

instance Primitive Float8 where
  typeName = Tagged "float8"
  baseOid = Tagged 701
  arrayOid = Tagged 1022
  binaryEncoder (Float8 x) = Write.bWord64 (castDoubleToWord64 x)
  binaryDecoder = PeekyBlinders.statically (Right . Float8 . castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
  textualEncoder (Float8 x) = TextBuilder.string (show x)
