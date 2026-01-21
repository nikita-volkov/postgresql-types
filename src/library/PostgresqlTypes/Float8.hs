module PostgresqlTypes.Float8
  ( Float8,

    -- * Accessors
    toDouble,

    -- * Constructors
    fromDouble,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @float8@ type. 8-byte floating-point number. 15 decimal digits precision.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-numeric.html#DATATYPE-FLOAT).
newtype Float8 = Float8 Double
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsScalar Float8)

instance IsScalar Float8 where
  typeName = Tagged "float8"
  baseOid = Tagged (Just 701)
  arrayOid = Tagged (Just 1022)
  typeParams = Tagged []
  binaryEncoder (Float8 x) = Write.bWord64 (castDoubleToWord64 x)
  binaryDecoder = PtrPeeker.fixed (Right . Float8 . castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
  textualEncoder (Float8 x) = TextBuilder.string (printf "%g" x)
  textualDecoder =
    (Float8 (0 / 0) <$ Attoparsec.string "NaN")
      <|> (Float8 (1 / 0) <$ Attoparsec.string "Infinity")
      <|> (Float8 (-1 / 0) <$ Attoparsec.string "-Infinity")
      <|> (Float8 <$> Attoparsec.double)

-- * Accessors

-- | Extract the underlying 'Double' value.
toDouble :: Float8 -> Double
toDouble (Float8 d) = d

-- * Constructors

-- | Construct a PostgreSQL 'Float8' from a 'Double' value.
fromDouble :: Double -> Float8
fromDouble = Float8
