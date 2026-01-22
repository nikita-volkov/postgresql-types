module PostgresqlTypes.Uuid
  ( Uuid (..),

    -- * Accessors
    toUUID,

    -- * Constructors
    fromUUID,
  )
where

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
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-uuid.html).
newtype Uuid = Uuid Data.UUID.UUID
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsScalar Uuid)

instance IsScalar Uuid where
  typeName = Tagged "uuid"
  baseOid = Tagged (Just 2950)
  arrayOid = Tagged (Just 2951)
  typeParams = Tagged []
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

-- * Accessors

-- | Extract the underlying 'Data.UUID.UUID' value.
toUUID :: Uuid -> Data.UUID.UUID
toUUID (Uuid uuid) = uuid

-- * Constructors

-- | Construct a PostgreSQL 'Uuid' from a 'Data.UUID.UUID' value.
fromUUID :: Data.UUID.UUID -> Uuid
fromUUID = Uuid
