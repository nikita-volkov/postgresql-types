module PostgresqlTypes.Int2
  ( Int2 (..),

    -- * Accessors
    toInt16,

    -- * Constructors
    fromInt16,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @int2@ type. 2-byte signed integer.
--
-- Range: @-32768@ to @+32767@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-numeric.html#DATATYPE-INT).
newtype Int2 = Int2 Int16
  deriving newtype (Eq, Ord, Hashable, Arbitrary)
  deriving (Show, Read, IsString) via (ViaIsScalar Int2)

instance IsScalar Int2 where
  schemaName = Tagged Nothing
  typeName = Tagged "int2"
  baseOid = Tagged (Just 21)
  arrayOid = Tagged (Just 1005)
  typeParams = Tagged []
  binaryEncoder (Int2 x) = Write.bInt16 x
  binaryDecoder = PtrPeeker.fixed (Right . Int2 <$> PtrPeeker.beSignedInt2)
  textualEncoder (Int2 x) = TextBuilder.decimal x
  textualDecoder = Int2 <$> Attoparsec.signed Attoparsec.decimal

-- * Accessors

-- | Extract the underlying 'Int16' value.
toInt16 :: Int2 -> Int16
toInt16 (Int2 i) = i

-- * Constructors

-- | Construct a PostgreSQL 'Int2' from an 'Int16' value.
fromInt16 :: Int16 -> Int2
fromInt16 = Int2
