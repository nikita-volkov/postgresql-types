module PostgresqlTypes.Int2 (Int2) where

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
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsScalar Int2)

instance IsScalar Int2 where
  typeName = Tagged "int2"
  baseOid = Tagged (Just 21)
  arrayOid = Tagged (Just 1005)
  typeParams = Tagged []
  binaryEncoder (Int2 x) = Write.bInt16 x
  binaryDecoder = PtrPeeker.fixed (Right . Int2 <$> PtrPeeker.beSignedInt2)
  textualEncoder (Int2 x) = TextBuilder.decimal x
  textualDecoder = Int2 <$> Attoparsec.signed Attoparsec.decimal

-- | Direct conversion from 'Int16'.
-- This is always safe since both types represent 16-bit signed integers identically.
instance IsSome Int16 Int2 where
  to (Int2 i) = i
  maybeFrom = Just . Int2

-- | Direct conversion from PostgreSQL Int2 to 'Int16'.
-- This is always safe since both types represent 16-bit signed integers identically.
instance IsSome Int2 Int16 where
  to i = Int2 i
  maybeFrom (Int2 i) = Just i

-- | Direct conversion from 'Int16'.
-- This is a total conversion as it always succeeds.
instance IsMany Int16 Int2 where
  onfrom = Int2

-- | Direct conversion from PostgreSQL Int2 to 'Int16'.
-- This is a total conversion as it always succeeds.
instance IsMany Int2 Int16 where
  onfrom (Int2 i) = i

-- | Bidirectional conversion between 'Int16' and PostgreSQL Int2.
instance Is Int16 Int2

instance Is Int2 Int16
