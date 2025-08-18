-- | PostgreSQL @uuid@ type.
-- Represents a Universally Unique Identifier in PostgreSQL.
module PrimitiveLayer.Primitives.Uuid (Uuid) where

import qualified Data.UUID
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @uuid@ type wrapper around 'Data.UUID.UUID'.
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

-- | Direct conversion from 'Data.UUID.UUID'.
-- This is a total conversion as it always succeeds.
instance IsMany Data.UUID.UUID Uuid where
  from = Uuid
