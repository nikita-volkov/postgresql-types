{-# OPTIONS_GHC -Wno-orphans #-}

module PrimitiveLayer.Primitives.Uuid (UUID) where

import qualified Data.UUID as Uuid
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

instance Primitive UUID where
  typeName = Tagged "uuid"
  baseOid = Tagged 2950
  arrayOid = Tagged 2951
  binaryEncoder uuid =
    case Uuid.toWords uuid of
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
          <$> ( Uuid.fromWords
                  <$> PeekyBlinders.beUnsignedInt4
                  <*> PeekyBlinders.beUnsignedInt4
                  <*> PeekyBlinders.beUnsignedInt4
                  <*> PeekyBlinders.beUnsignedInt4
              )
      )
  textualEncoder = TextBuilder.text . Uuid.toText
