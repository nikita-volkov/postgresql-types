module PostgresqlTypes.Oid
  ( Oid (..),

    -- * Accessors
    toWord32,

    -- * Constructors
    fromWord32,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Hashable (Hashable)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @oid@ type. Object identifier.
--
-- Range: @0@ to @4294967295@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-oid.html).
newtype Oid = Oid Word32
  deriving newtype (Eq, Ord, Hashable, Arbitrary)
  deriving (Show, Read, IsString) via (ViaIsScalar Oid)

instance IsScalar Oid where
  schemaName = Tagged Nothing
  typeName = Tagged "oid"
  baseOid = Tagged (Just 26)
  arrayOid = Tagged (Just 1028)
  typeParams = Tagged []
  binaryEncoder (Oid x) = Write.bWord32 x
  binaryDecoder = PtrPeeker.fixed (Right . Oid <$> PtrPeeker.beUnsignedInt4)
  textualEncoder (Oid x) = TextBuilder.decimal x
  textualDecoder = Oid <$> Attoparsec.decimal

-- * Accessors

-- | Extract the underlying 'Word32' value.
toWord32 :: Oid -> Word32
toWord32 (Oid w) = w

-- * Constructors

-- | Construct a PostgreSQL 'Oid' from a 'Word32' value.
fromWord32 :: Word32 -> Oid
fromWord32 = Oid
