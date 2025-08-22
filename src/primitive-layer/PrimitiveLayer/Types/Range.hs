module PrimitiveLayer.Types.Range where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PrimitiveLayer.Writes as Writes
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

data Range a
  = EmptyRange
  | BoundedRange (RangeBound a) (RangeBound a)
  deriving stock (Eq, Functor)
  deriving (Show) via (ViaPrimitive (Range a))

data RangeBound a = RangeBound
  { -- | Whether the bound is inclusive or exclusive.
    inclusive :: Bool,
    -- | Nothing when infinity.
    value :: Maybe a
  }
  deriving stock (Eq, Show, Functor)

instance (RangeMapping a, Ord a) => Mapping (Range a) where
  typeName = retag @a rangeTypeName
  baseOid = retag @a rangeOid
  arrayOid = retag @a rangeArrayOid
  binaryEncoder = \case
    EmptyRange ->
      Write.word8 0b00000001
    BoundedRange (RangeBound lowerInclusive lowerValue) (RangeBound upperInclusive upperValue) ->
      mconcat
        [ Write.word8
            ( 0
                .|. (if lowerInclusive then 0b00000010 else 0)
                .|. (if upperInclusive then 0b00000100 else 0)
                .|. (if null lowerValue then 0b00001000 else 0)
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
      PeekyBlinders.statically PeekyBlinders.unsignedInt1
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
        if upperValue == lowerValue
          then when (not (lowerInclusive && upperInclusive)) do
            throwError (DecodingError ["range"] (InvalidValueDecodingErrorReason "When lower and upper bounds are equal, both must be inclusive" (TextBuilder.toText (TextBuilder.decimal flags))))
          else case (lowerValue, upperValue) of
            (Just lv, Just uv) ->
              when (lv > uv) do
                throwError (DecodingError ["range"] (InvalidValueDecodingErrorReason "Lower bound must not be greater than upper bound" (TextBuilder.toText (TextBuilder.decimal flags))))
            _ -> pure ()
        pure (BoundedRange (RangeBound lowerInclusive lowerValue) (RangeBound upperInclusive upperValue))
    where
      decodeBound infinite =
        if infinite
          then pure Nothing
          else
            Just <$> do
              size <- lift do
                PeekyBlinders.statically PeekyBlinders.beSignedInt4
              when (size < 0) do
                throwError (DecodingError ["bound-size"] (InvalidValueDecodingErrorReason "Expecting >= 0" (TextBuilder.toText (TextBuilder.decimal size))))
              ExceptT do
                PeekyBlinders.forceSize (fromIntegral size) do
                  binaryDecoder @a

  textualEncoder = \case
    EmptyRange -> "empty"
    BoundedRange (RangeBound lowerInclusive lowerValue) (RangeBound upperInclusive upperValue) ->
      mconcat
        [ if lowerInclusive then "[" else "(",
          foldMap textualEncoder lowerValue,
          ",",
          foldMap textualEncoder upperValue,
          if upperInclusive then "]" else ")"
        ]

instance (Arbitrary a, Ord a, Enum a) => Arbitrary (Range a) where
  arbitrary =
    QuickCheck.frequency
      [ (1, pure EmptyRange),
        ( 10,
          do
            (lowerValue, upperValue) <- do
              value1 <- QuickCheck.frequency [(1, pure Nothing), (10, Just <$> arbitrary)]
              value2 <-
                QuickCheck.frequency
                  [ (1, pure Nothing),
                    ( 10,
                      Just <$> case value1 of
                        Nothing -> arbitrary
                        Just v -> QuickCheck.suchThat arbitrary (/= v)
                    )
                  ]
              pure if value1 < value2 then (value1, value2) else (value2, value1)
            lowerInclusive <- arbitrary
            upperInclusive <-
              if fmap succ lowerValue == upperValue && not lowerInclusive
                then pure True
                else arbitrary
            pure (BoundedRange (RangeBound lowerInclusive lowerValue) (RangeBound upperInclusive upperValue))
        )
      ]
