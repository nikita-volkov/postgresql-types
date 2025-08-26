module PrimitiveLayer.Types.Range (Range) where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PrimitiveLayer.Writes as Writes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- |
-- Normalized representation of a range in the @[inclusiveLowerBound,exclusiveUpperBound)@ form.
--
-- Although PostgreSQL has the concept of inclusive and exclusive bounds in ranges in reality it always normalizes the range values to one form.
-- The lower bound is inclusive and the upper bound is exclusive with one exception: if the lower bound is infinity then it is treated as exclusive.
-- There is also another special value: empty.
--
-- The following standard types are supported via the 'RangeMapping' instances:
--
-- - @int4range@ - @Range Int4@
-- - @int8range@ - @Range Int8@
-- - @numrange@ - @Range Numeric@
-- - @tsrange@ - @Range Timestamp@
-- - @tstzrange@ - @Range Timestamptz@
-- - @daterange@ - @Range Date@
--
-- You can also define your own.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/rangetypes.html).
data Range a
  = EmptyRange
  | BoundedRange (Maybe a) (Maybe a)
  deriving stock (Eq, Functor)
  deriving (Show) via (ViaPrimitive (Range a))

instance (RangeMapping a, Ord a) => Mapping (Range a) where
  typeName = retag @a rangeTypeName
  baseOid = retag @a rangeOid
  arrayOid = retag @a rangeArrayOid
  binaryEncoder = \case
    EmptyRange ->
      Write.word8 0b00000001
    BoundedRange lowerValue upperValue ->
      mconcat
        [ Write.word8
            ( (if null lowerValue then 0b00001000 else 0b00000010)
                .|. (if null upperValue then 0b00010000 else 0)
            ),
          foldMap renderBound lowerValue,
          foldMap renderBound upperValue
        ]
    where
      renderBound bound =
        Writes.sized (binaryEncoder bound)

  binaryDecoder = runExceptT do
    flags <- lift do
      PtrPeeker.fixed PtrPeeker.unsignedInt1
    let emptyRange = testBit flags 0
        lowerInclusive = testBit flags 1
        upperInclusive = testBit flags 2
        lowerInfinite = testBit flags 3
        upperInfinite = testBit flags 4
    if emptyRange
      then pure EmptyRange
      else do
        lowerValue <- decodeBound lowerInfinite
        upperValue <- decodeBound upperInfinite

        when (isJust lowerValue && not lowerInclusive) do
          throwError (DecodingError ["lower-value"] (InvalidValueDecodingErrorReason "Lower bound cannot be exclusive when it is not infinite" (TextBuilder.toText (TextBuilder.decimal flags))))

        when (isJust upperValue && upperInclusive) do
          throwError (DecodingError ["upper-value"] (InvalidValueDecodingErrorReason "Upper bound cannot be inclusive" (TextBuilder.toText (TextBuilder.decimal flags))))

        case (lowerValue, upperValue) of
          (Just lv, Just uv) ->
            when (lv >= uv) do
              throwError (DecodingError ["upper-value"] (InvalidValueDecodingErrorReason "Upper value is smaller than the lower one" (TextBuilder.toText (TextBuilder.decimal flags))))
          _ -> pure ()

        pure (BoundedRange lowerValue upperValue)
    where
      decodeBound infinite =
        if infinite
          then pure Nothing
          else
            Just <$> do
              size <- lift do
                PtrPeeker.fixed PtrPeeker.beSignedInt4
              when (size < 0) do
                throwError (DecodingError ["bound-size"] (InvalidValueDecodingErrorReason "Expecting >= 0" (TextBuilder.toText (TextBuilder.decimal size))))
              ExceptT do
                PtrPeeker.forceSize (fromIntegral size) do
                  binaryDecoder @a

  textualEncoder = \case
    EmptyRange -> "empty"
    BoundedRange lowerValue upperValue ->
      mconcat
        [ case lowerValue of
            Nothing -> "("
            Just lowerValue -> "[" <> textualEncoder lowerValue,
          ",",
          case upperValue of
            Nothing -> ")"
            Just upperValue -> textualEncoder upperValue <> ")"
        ]

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
  arbitrary =
    QuickCheck.frequency
      [ (1, pure EmptyRange),
        ( 10,
          do
            value1 <- QuickCheck.frequency [(1, pure Nothing), (10, Just <$> arbitrary)]
            value2 <-
              QuickCheck.frequency
                [ (1, pure Nothing),
                  ( 10,
                    Just <$> case value1 of
                      Nothing -> arbitrary
                      Just value1 -> QuickCheck.suchThat arbitrary (/= value1)
                  )
                ]
            pure if value1 < value2 then BoundedRange value1 value2 else BoundedRange value2 value1
        )
      ]

-- |
-- Interprets values of @(Maybe (Maybe a, Maybe a))@ in the following way:
--
-- - @Nothing@ - empty range (@empty@)
-- - @Just (Nothing, Nothing)@ - infinity to infinity (@(,)@)
-- - @Just (Just lower, Just upper)@ - bounded range (@[lower, upper)@)
-- - @Just (Just lower, Nothing)@ - half-open range (@[lower,)@)
-- - @Just (Nothing, Just upper)@ - half-open range (@(,upper)@)
instance (Ord a) => IsSome (Maybe (Maybe a, Maybe a)) (Range a) where
  to = \case
    EmptyRange -> Nothing
    BoundedRange lower upper -> Just (lower, upper)

  maybeFrom = \case
    Just (Just lower, Just upper) -> if lower < upper then Just (BoundedRange (Just lower) (Just upper)) else Nothing
    Just (lower, upper) -> Just (BoundedRange lower upper)
    Nothing -> Just EmptyRange

-- | Normalizes ranges with invalid bounds (lower >= upper) to empty range.
instance (Ord a) => IsMany (Maybe (Maybe a, Maybe a)) (Range a) where
  onfrom = \case
    Just (Just lower, Just upper) -> if lower < upper then BoundedRange (Just lower) (Just upper) else EmptyRange
    Just (lower, upper) -> BoundedRange lower upper
    Nothing -> EmptyRange
