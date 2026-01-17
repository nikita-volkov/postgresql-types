module PostgresqlTypes.Types.Oid (Oid) where

import qualified Data.Attoparsec.Text as Attoparsec
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
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsScalar Oid)

instance IsScalar Oid where
  typeName = Tagged "oid"
  baseOid = Tagged (Just 26)
  arrayOid = Tagged (Just 1028)
  typeParams = Tagged []
  binaryEncoder (Oid x) = Write.bWord32 x
  binaryDecoder = PtrPeeker.fixed (Right . Oid <$> PtrPeeker.beUnsignedInt4)
  textualEncoder (Oid x) = TextBuilder.decimal x
  textualDecoder = Oid <$> Attoparsec.decimal

-- | Direct conversion from 'Word32'.
-- This is always safe since both types represent 32-bit unsigned integers identically.
instance IsSome Word32 Oid where
  to (Oid w) = w
  maybeFrom = Just . Oid

-- | Direct conversion from PostgreSQL Oid to 'Word32'.
-- This is always safe since both types represent 32-bit unsigned integers identically.
instance IsSome Oid Word32 where
  to w = Oid w
  maybeFrom (Oid w) = Just w

-- | Direct conversion from 'Word32'.
-- This is a total conversion as it always succeeds.
instance IsMany Word32 Oid where
  onfrom = Oid

-- | Direct conversion from PostgreSQL Oid to 'Word32'.
-- This is a total conversion as it always succeeds.
instance IsMany Oid Word32 where
  onfrom (Oid w) = w

-- | Bidirectional conversion between 'Word32' and PostgreSQL Oid.
instance Is Word32 Oid

instance Is Oid Word32
