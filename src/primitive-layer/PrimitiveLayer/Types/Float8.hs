module PrimitiveLayer.Types.Float8 (Float8) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @float8@ type. 8-byte floating-point number. 15 decimal digits precision.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-FLOAT).
newtype Float8 = Float8 Double
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Float8)

instance Mapping Float8 where
  typeName = Tagged "float8"
  baseOid = Tagged 701
  arrayOid = Tagged 1022
  binaryEncoder (Float8 x) = Write.bWord64 (castDoubleToWord64 x)
  binaryDecoder = PeekyBlinders.statically (Right . Float8 . castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
  textualEncoder (Float8 x) = TextBuilder.string (show x)

-- | Direct conversion from 'Double'.
-- This is always safe since both types represent 64-bit floating point numbers identically.
instance IsSome Double Float8 where
  to (Float8 d) = d
  maybeFrom = Just . Float8

-- | Direct conversion from PostgreSQL Float8 to 'Double'.
-- This is always safe since both types represent 64-bit floating point numbers identically.
instance IsSome Float8 Double where
  to d = Float8 d
  maybeFrom (Float8 d) = Just d

-- | Direct conversion from 'Double'.
-- This is a total conversion as it always succeeds.
instance IsMany Double Float8 where
  onfrom = Float8

-- | Direct conversion from PostgreSQL Float8 to 'Double'.
-- This is a total conversion as it always succeeds.
instance IsMany Float8 Double where
  onfrom (Float8 d) = d

-- | Bidirectional conversion between 'Double' and PostgreSQL Float8.
instance Is Double Float8

instance Is Float8 Double
