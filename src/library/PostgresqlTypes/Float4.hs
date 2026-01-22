module PostgresqlTypes.Float4
  ( Float4 (..),

    -- * Accessors
    toFloat,

    -- * Constructors
    fromFloat,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Hashable (Hashable)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @float4@ type. 4-byte floating-point number. 6 decimal digits precision.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-numeric.html#DATATYPE-FLOAT).
newtype Float4 = Float4 Float
  deriving newtype (Eq, Ord, Hashable, Arbitrary)
  deriving (Show, Read, IsString) via (ViaIsScalar Float4)

instance IsScalar Float4 where
  typeName = Tagged "float4"
  baseOid = Tagged (Just 700)
  arrayOid = Tagged (Just 1021)
  typeParams = Tagged []
  binaryEncoder (Float4 x) = Write.bWord32 (castFloatToWord32 x)
  binaryDecoder = PtrPeeker.fixed (Right . Float4 . castWord32ToFloat <$> PtrPeeker.beUnsignedInt4)
  textualEncoder (Float4 x) = TextBuilder.string (printf "%g" x)
  textualDecoder =
    (Float4 (0 / 0) <$ Attoparsec.string "NaN")
      <|> (Float4 (1 / 0) <$ Attoparsec.string "Infinity")
      <|> (Float4 (-1 / 0) <$ Attoparsec.string "-Infinity")
      <|> (Float4 . realToFrac <$> Attoparsec.double)

-- * Accessors

-- | Extract the underlying 'Float' value.
toFloat :: Float4 -> Float
toFloat (Float4 f) = f

-- * Constructors

-- | Construct a PostgreSQL 'Float4' from a 'Float' value.
fromFloat :: Float -> Float4
fromFloat = Float4
