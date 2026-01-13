module PostgresqlTypes.Codec.Dimensionality where

import PostgresqlTypes.Codec.DecodingError
import PostgresqlTypes.Codec.Dimension
import qualified PostgresqlTypes.Codec.Dimensionality.Writes as Writes
import PostgresqlTypes.Codec.Nullability
import PostgresqlTypes.Codec.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Dimensionality scalar vec = Dimensionality
  { binaryEncoder ::
      Word32 ->
      (scalar -> Write.Write) ->
      (vec -> Write.Write),
    binaryDecoder ::
      Word32 ->
      PtrPeeker.Variable (Either DecodingError scalar) ->
      PtrPeeker.Variable (Either DecodingError vec),
    textualEncoder ::
      (scalar -> TextBuilder.TextBuilder) ->
      (vec -> TextBuilder.TextBuilder)
  }

d0 :: Dimensionality a a
d0 =
  Dimensionality
    { binaryEncoder = const id,
      binaryDecoder = const id,
      textualEncoder = id
    }

d1 :: Nullability a b -> Dimension b c -> Dimensionality a c
d1 elementNullability (Dimension construct1 destruct1 count1) =
  Dimensionality
    { binaryEncoder =
        \baseOid scalarEncoder vec ->
          let head =
                Writes.arrayHeader 1 elementNullability.nullable baseOid [count1 vec]
              elementEncoder =
                elementNullability.binaryEncoderWrapper scalarEncoder
              body =
                destruct1
                  ( \accumulator element ->
                      accumulator <> elementEncoder element
                  )
                  head
                  vec
           in head <> body,
      binaryDecoder =
        \expectedBaseOid scalarDecoder -> runExceptT do
          (dimensionCount, hasNulls, baseOid) <- lift do
            PtrPeeker.fixed do
              dimensionCount <- PtrPeeker.beSignedInt4
              hasNulls <- do
                int <- PtrPeeker.beSignedInt4
                pure (int == 1)
              baseOid <- PtrPeeker.beUnsignedInt4
              pure (dimensionCount, hasNulls, baseOid)

          -- Interrupt early if baseOid does not match the expected one.
          when (baseOid /= expectedBaseOid) do
            throwError
              DecodingError
                { location = ["base-oid"],
                  reason =
                    UnexpectedValueDecodingErrorReason
                      (TextBuilder.toText (TextBuilder.decimal expectedBaseOid))
                      (TextBuilder.toText (TextBuilder.decimal baseOid))
                }

          -- Interrupt early if the amount of dimensions is wrong.
          when (dimensionCount /= 1) do
            throwError
              DecodingError
                { location = ["dimension-count"],
                  reason =
                    UnexpectedValueDecodingErrorReason
                      "1"
                      (TextBuilder.toText (TextBuilder.decimal dimensionCount))
                }

          -- Interrupt early if the array contains nulls and they are not expected.
          when (hasNulls && not elementNullability.nullable) do
            throwError
              DecodingError
                { location = ["contains-nulls"],
                  reason = UnexpectedValueDecodingErrorReason "false" "true"
                }

          (size1, lowerBound1) <- lift do
            PtrPeeker.fixed do
              liftA2 (,) PtrPeeker.beSignedInt4 PtrPeeker.beSignedInt4

          -- Interrupt early if the lower-bound value is not supported.
          when (lowerBound1 /= 1) do
            throwError
              DecodingError
                { location = ["lower-bound"],
                  reason =
                    UnexpectedValueDecodingErrorReason
                      "1"
                      (TextBuilder.toText (TextBuilder.decimal lowerBound1))
                }

          construct1 (fromIntegral size1) do
            ExceptT do
              elementNullability.binaryDecoderWrapper scalarDecoder,
      textualEncoder = \_scalarEncoder _vec ->
        error "TODO"
    }

d2 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimensionality a d
d2 = error "TODO"
