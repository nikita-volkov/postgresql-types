module PostgresqlTypes.Types.Float4 (Float4) where

import qualified Data.Attoparsec.Text as Attoparsec
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @float4@ type. 4-byte floating-point number. 6 decimal digits precision.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-FLOAT).
newtype Float4 = Float4 Float
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsStandardType Float4)

instance IsStandardType Float4 where
  typeIdsOf =
    TypeIdsOf
      { name = "float4",
        stableBaseOid = Just 700,
        stableArrayOid = Just 1021
      }
  binaryEncoder (Float4 x) = Write.bWord32 (castFloatToWord32 x)
  binaryDecoder = PtrPeeker.fixed (Right . Float4 . castWord32ToFloat <$> PtrPeeker.beUnsignedInt4)
  textualEncoder (Float4 x) = TextBuilder.string (printf "%g" x)
  textualDecoder =
    (Float4 (0 / 0) <$ Attoparsec.string "NaN")
      <|> (Float4 (1 / 0) <$ Attoparsec.string "Infinity")
      <|> (Float4 (-1 / 0) <$ Attoparsec.string "-Infinity")
      <|> (Float4 . realToFrac <$> Attoparsec.double)

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
  onfrom = Float4

-- | Direct conversion from PostgreSQL Float4 to 'Float'.
-- This is a total conversion as it always succeeds.
instance IsMany Float4 Float where
  onfrom (Float4 f) = f

-- | Bidirectional conversion between 'Float' and PostgreSQL Float4.
instance Is Float Float4

instance Is Float4 Float
