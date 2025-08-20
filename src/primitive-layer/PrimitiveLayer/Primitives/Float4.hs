-- | @float4@. 4-byte floating-point number. 6 decimal digits precision.
module PrimitiveLayer.Primitives.Float4 (Float4 (..)) where

import Data.Bits
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @float4@ type wrapper around 'Float'.
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

-- | Direct conversion from 'Float'.
-- This is always safe since both types represent 32-bit floating point numbers identically.
instance IsSome Float Float4 where
  to (Float4 f) = f
  maybeFrom = Just . Float4

-- | Direct conversion from PostgreSQL Float4 to 'Float'.
-- This is always safe since both types represent 32-bit floating point numbers identically.
instance IsSome Float4 Float where
  to f = Float4 f
  maybeFrom (Float4 f) = Just f

-- | Direct conversion from 'Float'.
-- This is a total conversion as it always succeeds.
instance IsMany Float Float4 where
  from = Float4

-- | Direct conversion from PostgreSQL Float4 to 'Float'.
-- This is a total conversion as it always succeeds.
instance IsMany Float4 Float where
  from (Float4 f) = f

-- | Bidirectional conversion between 'Float' and PostgreSQL Float4.
instance Is Float Float4

instance Is Float4 Float
