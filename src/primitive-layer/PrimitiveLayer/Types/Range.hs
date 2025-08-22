module PrimitiveLayer.Types.Range where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PrimitiveLayer.Writes as Writes
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Bound a = Incl a | Excl a | Inf
  deriving stock (Eq, Show, Ord)

data Range a = Empty | Range (Bound a) (Bound a)
  deriving stock (Eq, Show, Ord)

instance (RangeMapping a) => Mapping (Range a) where
  typeName = retag @a rangeTypeName
  baseOid = retag @a rangeOid
  arrayOid = retag @a rangeArrayOid
  binaryEncoder = \case
    Empty -> Write.word8 0x01
    Range Inf Inf -> Write.word8 0x18
    Range (Excl l) (Excl r) -> Write.word8 0x00 <> Writes.sized (binaryEncoder l) <> Writes.sized (binaryEncoder r)
    Range (Incl l) (Excl r) -> Write.word8 0x02 <> Writes.sized (binaryEncoder l) <> Writes.sized (binaryEncoder r)
    Range (Excl l) (Incl r) -> Write.word8 0x04 <> Writes.sized (binaryEncoder l) <> Writes.sized (binaryEncoder r)
    Range (Incl l) (Incl r) -> Write.word8 0x06 <> Writes.sized (binaryEncoder l) <> Writes.sized (binaryEncoder r)
    Range (Excl l) Inf -> Write.word8 0x10 <> Writes.sized (binaryEncoder l)
    Range (Incl l) Inf -> Write.word8 0x12 <> Writes.sized (binaryEncoder l)
    Range Inf (Excl r) -> Write.word8 0x08 <> Writes.sized (binaryEncoder r)
    Range Inf (Incl r) -> Write.word8 0x0C <> Writes.sized (binaryEncoder r)
  binaryDecoder = runExceptT do
    flags <- lift do
      PeekyBlinders.statically PeekyBlinders.unsignedInt1
    let emptyRange = testBit flags 0
        lowerInclusive = testBit flags 1
        upperInclusive = testBit flags 2
        lowerInfinite = testBit flags 3
        upperInfinite = testBit flags 4
    if
      | emptyRange ->
          pure $ Empty
      | lowerInfinite && upperInfinite ->
          pure $ Range Inf Inf
      | lowerInfinite ->
          Range Inf <$> decodeBound upperInclusive
      | upperInfinite ->
          Range <$> decodeBound lowerInclusive <*> pure Inf
      | otherwise ->
          Range <$> decodeBound lowerInclusive <*> decodeBound upperInclusive
    where
      decodeBound isIncl = do
        size <- lift do
          PeekyBlinders.statically PeekyBlinders.beSignedInt2
        when (size < 0) do
          throwError (DecodingError ["bound-size"] (InvalidValueDecodingErrorReason "Expecting >= 0" (TextBuilder.toText (TextBuilder.decimal size))))
        content <- ExceptT do
          PeekyBlinders.forceSize (fromIntegral size) do
            binaryDecoder @a
        pure (if isIncl then Incl content else Excl content)
  textualEncoder = error "Not implemented: textualEncoder for Range"
