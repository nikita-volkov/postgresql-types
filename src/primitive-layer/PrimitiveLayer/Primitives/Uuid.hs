module PrimitiveLayer.Primitives.Uuid (Uuid) where

import qualified Data.UUID
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @uuid@ type. Universally unique identifier.
--
-- Isomorphic to 'Data.UUID.UUID'.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-uuid.html)
newtype Uuid = Uuid Data.UUID.UUID
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Uuid)

instance Primitive Uuid where
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
  binaryDecoder = PeekyBlinders.statically do
    Right
      . Uuid
      <$> ( Data.UUID.fromWords
              <$> PeekyBlinders.beUnsignedInt4
              <*> PeekyBlinders.beUnsignedInt4
              <*> PeekyBlinders.beUnsignedInt4
              <*> PeekyBlinders.beUnsignedInt4
          )
  textualEncoder = TextBuilder.text . Data.UUID.toText . coerce

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
  from = Uuid

-- | Direct conversion from PostgreSQL Uuid to 'Data.UUID.UUID'.
-- This is a total conversion as it always succeeds.
instance IsMany Uuid Data.UUID.UUID where
  from (Uuid uuid) = uuid

-- | Bidirectional conversion between 'Data.UUID.UUID' and PostgreSQL Uuid.
instance Is Data.UUID.UUID Uuid

instance Is Uuid Data.UUID.UUID
