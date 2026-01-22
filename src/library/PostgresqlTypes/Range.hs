module PostgresqlTypes.Range
  ( Range,

    -- * Accessors
    isEmpty,
    fold,

    -- * Constructors
    empty,
    unbounded,
    normalizeBounded,
    refineBounded,

    -- ** Combinators
    mergeIfOverlappingOrAdjacent,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (empty, fold)
import PostgresqlTypes.Via
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
-- The following standard types are supported via the 'IsRangeElement' instances:
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
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/rangetypes.html).
data Range a
  = EmptyRange
  | BoundedRange (Maybe a) (Maybe a)
  deriving stock (Eq, Functor)
  deriving (Show, Read, IsString) via (ViaIsScalar (Range a))

instance (IsRangeElement a) => IsScalar (Range a) where
  schemaName = Tagged Nothing
  typeName = retag (rangeTypeName @a)
  baseOid = retag (rangeBaseOid @a)
  arrayOid = retag (rangeArrayOid @a)
  typeParams = retag (typeParams @a)
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
        let write = binaryEncoder bound
         in Write.bWord32 (fromIntegral (Write.writeSize write)) <> write

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
          throwError (DecodingError ["lower-value"] (UnsupportedValueDecodingErrorReason "Lower bound cannot be exclusive when it is not infinite" (TextBuilder.toText (TextBuilder.decimal flags))))

        when (isJust upperValue && upperInclusive) do
          throwError (DecodingError ["upper-value"] (UnsupportedValueDecodingErrorReason "Upper bound cannot be inclusive" (TextBuilder.toText (TextBuilder.decimal flags))))

        case (lowerValue, upperValue) of
          (Just lv, Just uv) ->
            when (lv >= uv) do
              throwError (DecodingError ["upper-value"] (UnsupportedValueDecodingErrorReason "Upper value is smaller than the lower one" (TextBuilder.toText (TextBuilder.decimal flags))))
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
                throwError (DecodingError ["bound-size"] (UnsupportedValueDecodingErrorReason "Expecting >= 0" (TextBuilder.toText (TextBuilder.decimal size))))
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
  textualDecoder =
    parseEmpty <|> parseBounded
    where
      parseEmpty = EmptyRange <$ Attoparsec.string "empty"
      parseBounded = do
        lowerBracket <- Attoparsec.satisfy (\c -> c == '[' || c == '(')
        Attoparsec.skipSpace
        lowerValue <-
          if lowerBracket == '['
            then Just <$> parseElement
            else pure Nothing
        Attoparsec.skipSpace
        _ <- Attoparsec.char ','
        upperValue <- optional parseElement
        _ <- Attoparsec.char ')'
        pure (BoundedRange lowerValue upperValue)

      -- Parse element that might be quoted by PostgreSQL (for extreme dates)
      parseElement =
        quotedElement <|> textualDecoder @a
        where
          quotedElement = Attoparsec.char '"' *> textualDecoder @a <* Attoparsec.char '"'

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

instance (Ord a) => Ord (Range a) where
  compare EmptyRange EmptyRange = EQ
  compare EmptyRange (BoundedRange _ _) = LT
  compare (BoundedRange _ _) EmptyRange = GT
  compare (BoundedRange lower1 upper1) (BoundedRange lower2 upper2) =
    case compareBounds lower1 lower2 of
      EQ -> compareBounds upper1 upper2
      other -> other
    where
      compareBounds Nothing Nothing = EQ
      compareBounds Nothing (Just _) = LT
      compareBounds (Just _) Nothing = GT
      compareBounds (Just v1) (Just v2) = compare v1 v2

isEmpty :: Range a -> Bool
isEmpty = \case
  EmptyRange -> True
  BoundedRange _ _ -> False

-- | Pattern matching on 'Range'.
fold ::
  -- | Empty range case.
  b ->
  -- | Bounded range case.
  (Maybe a -> Maybe a -> b) ->
  (Range a -> b)
fold emptyCase boundedCase = \case
  EmptyRange -> emptyCase
  BoundedRange lower upper -> boundedCase lower upper

-- | Constructs an empty range.
empty :: Range a
empty = EmptyRange

-- | Constructs a range without bounds (infinity to infinity).
unbounded :: Range a
unbounded = BoundedRange Nothing Nothing

-- | Constructs a bounded range normalizing the bounds.
-- If the lower bound is not less than the upper bound, returns 'empty'.
normalizeBounded :: (Ord a) => Maybe a -> Maybe a -> Range a
normalizeBounded = \case
  Just lower -> \case
    Just upper ->
      if lower < upper
        then BoundedRange (Just lower) (Just upper)
        else EmptyRange
    Nothing -> BoundedRange (Just lower) Nothing
  Nothing -> \case
    Just upper -> BoundedRange Nothing (Just upper)
    Nothing -> BoundedRange Nothing Nothing

-- | Constructs a bounded range refining the bounds.
-- If the lower bound is not less than the upper bound, returns 'Nothing'.
refineBounded :: (Ord a) => Maybe a -> Maybe a -> Maybe (Range a)
refineBounded = \case
  Just lower -> \case
    Just upper ->
      if lower < upper
        then Just (BoundedRange (Just lower) (Just upper))
        else Nothing
    Nothing -> Just (BoundedRange (Just lower) Nothing)
  Nothing -> \case
    Just upper -> Just (BoundedRange Nothing (Just upper))
    Nothing -> Just (BoundedRange Nothing Nothing)

-- | Merge two ranges if they are overlapping or adjacent.
-- Returns 'Just' the merged range if they overlap or are adjacent, 'Nothing' otherwise.
--
-- Ranges are normalized as [lower, upper) (inclusive lower, exclusive upper).
-- Two ranges are adjacent if one ends exactly where the other begins.
-- Two ranges overlap if they share any values.
mergeIfOverlappingOrAdjacent :: (Ord a) => Range a -> Range a -> Maybe (Range a)
mergeIfOverlappingOrAdjacent = \case
  EmptyRange -> Just
  BoundedRange lower1 upper1 -> \case
    EmptyRange -> Just (BoundedRange lower1 upper1)
    BoundedRange lower2 upper2 ->
      -- Check if ranges overlap or are adjacent:
      -- r1 = [lower1, upper1), r2 = [lower2, upper2)
      -- They overlap or are adjacent if upper1 >= lower2
      case (upper1, lower2) of
        -- r1 extends to infinity, so always overlaps with or is adjacent to r2
        (Nothing, _) -> Just (BoundedRange lower1 Nothing)
        -- r2 starts from -infinity
        -- If we're processing in sorted order, this shouldn't happen
        -- (lower1 can't be >= -infinity when lower2 is -infinity)
        (Just _, Nothing) ->
          -- However, if it does occur, these ranges must overlap
          -- Result is (-infinity, max(upper1, upper2))
          Just (BoundedRange Nothing (maxBound upper1 upper2))
        (Just u1, Just l2) ->
          -- Both bounds are finite
          -- Overlapping: u1 > l2 (r1 extends into r2)
          -- Adjacent: u1 == l2 (r1 ends exactly where r2 begins)
          if u1 >= l2
            then Just (BoundedRange lower1 (maxBound upper1 upper2))
            else Nothing
  where
    -- Helper to get the max of two Maybe bounds (Nothing represents infinity)
    maxBound Nothing _ = Nothing
    maxBound _ Nothing = Nothing
    maxBound (Just a) (Just b) = Just (max a b)
