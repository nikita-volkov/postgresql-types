module PrimitiveLayer.Primitives.Uuid (Uuid (..)) where

import qualified Data.UUID
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

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
  binaryDecoder =
    PeekyBlinders.statically
      ( Right
          . Uuid
          <$> ( Data.UUID.fromWords
                  <$> PeekyBlinders.beUnsignedInt4
                  <*> PeekyBlinders.beUnsignedInt4
                  <*> PeekyBlinders.beUnsignedInt4
                  <*> PeekyBlinders.beUnsignedInt4
              )
      )
  textualEncoder = TextBuilder.text . Data.UUID.toText . coerce
