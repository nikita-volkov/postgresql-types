module PostgresqlTypes.Types.Uuid (Uuid) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.UUID
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @uuid@ type. Universally unique identifier.
--
-- Isomorphic to 'Data.UUID.UUID'.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-uuid.html).
newtype Uuid = Uuid Data.UUID.UUID
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsStandardType Uuid)

instance IsStandardType Uuid where
  typeName = Tagged "uuid"
  baseOid = Tagged 2950
  arrayOid = Tagged 2951
  binaryEncoder (Uuid uuid) =
    case Data.UUID.toWords uuid of
      (w1, w2, w3, w4) ->
        mconcat
          [ Write.bWord32 w1,
            Write.bWord32 w2,
            Write.bWord32 w3,
            Write.bWord32 w4
          ]
  binaryDecoder = PtrPeeker.fixed do
    (Right . Uuid)
      <$> ( Data.UUID.fromWords
              <$> PtrPeeker.beUnsignedInt4
              <*> PtrPeeker.beUnsignedInt4
              <*> PtrPeeker.beUnsignedInt4
              <*> PtrPeeker.beUnsignedInt4
          )
  textualEncoder = TextBuilder.text . Data.UUID.toText . coerce
  textualDecoder = do
    uuidText <- Attoparsec.takeText
    case Data.UUID.fromText uuidText of
      Nothing -> fail "Invalid UUID format"
      Just uuid -> pure (Uuid uuid)

-- | Direct conversion from 'Data.UUID.UUID'.
-- This is always safe since both types represent UUIDs identically.
instance IsSome Data.UUID.UUID Uuid where
  to (Uuid uuid) = uuid
  maybeFrom = Just . Uuid

-- | Direct conversion from PostgreSQL Uuid to 'Data.UUID.UUID'.
-- This is always safe since both types represent UUIDs identically.
instance IsSome Uuid Data.UUID.UUID where
  to uuid = Uuid uuid
  maybeFrom (Uuid uuid) = Just uuid

-- | Direct conversion from 'Data.UUID.UUID'.
-- This is a total conversion as it always succeeds.
instance IsMany Data.UUID.UUID Uuid where
  onfrom = Uuid

-- | Direct conversion from PostgreSQL Uuid to 'Data.UUID.UUID'.
-- This is a total conversion as it always succeeds.
instance IsMany Uuid Data.UUID.UUID where
  onfrom (Uuid uuid) = uuid

-- | Bidirectional conversion between 'Data.UUID.UUID' and PostgreSQL Uuid.
instance Is Data.UUID.UUID Uuid

instance Is Uuid Data.UUID.UUID
