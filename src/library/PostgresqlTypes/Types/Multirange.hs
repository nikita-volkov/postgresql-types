module PostgresqlTypes.Types.Multirange (Multirange) where

import qualified BaseExtras.List
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Types.Range (Range)
import PostgresqlTypes.Via
import qualified PostgresqlTypes.Writes as Writes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified QuickCheckExtras.Gen
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- |
-- Normalized representation of a multirange, which is a collection of non-overlapping, non-adjacent ranges.
--
-- PostgreSQL multirange types store multiple ranges as a single value, automatically normalizing them
-- by combining overlapping and adjacent ranges. The ranges are stored in sorted order.
--
-- The following standard types are supported via the 'IsMultirangeElement' instances:
--
-- - @int4multirange@ - @Multirange Int4@
-- - @int8multirange@ - @Multirange Int8@
-- - @nummultirange@ - @Multirange Numeric@
-- - @tsmultirange@ - @Multirange Timestamp@
-- - @tstzmultirange@ - @Multirange Timestamptz@
-- - @datemultirange@ - @Multirange Date@
--
-- You can also define your own.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/rangetypes.html#RANGETYPES-MULTIRANGE).
newtype Multirange a = Multirange (Vector (Range a))
  deriving stock (Eq, Functor)
  deriving (Show) via (ViaIsScalar (Multirange a))

instance (IsMultirangeElement a) => IsScalar (Multirange a) where
  typeName = retag (multirangeTypeName @a)
  baseOid = retag (multirangeBaseOid @a)
  arrayOid = retag (multirangeArrayOid @a)
  typeParams = retag (typeParams @(Range a))
  binaryEncoder = \case
    Multirange ranges ->
      mconcat
        [ Write.bWord32 (fromIntegral (Vector.length ranges)),
          foldMap renderRange ranges
        ]
    where
      renderRange range =
        Writes.sized (binaryEncoder range)

  binaryDecoder = runExceptT do
    numRanges <- lift do
      PtrPeeker.fixed PtrPeeker.beUnsignedInt4
    ranges <- replicateM (fromIntegral numRanges) do
      size <- lift do
        PtrPeeker.fixed PtrPeeker.beSignedInt4
      when (size < 0) do
        throwError (DecodingError ["range-size"] (UnsupportedValueDecodingErrorReason "Expecting >= 0" (TextBuilder.toText (TextBuilder.decimal size))))
      ExceptT do
        PtrPeeker.forceSize (fromIntegral size) do
          binaryDecoder @(Range a)
    pure (Multirange (Vector.fromList ranges))

  textualEncoder = \case
    Multirange ranges ->
      mconcat
        [ "{",
          TextBuilder.intercalate "," (Vector.toList (Vector.map (textualEncoder @(Range a)) ranges)),
          "}"
        ]
  textualDecoder = do
    _ <- Attoparsec.char '{'
    Attoparsec.skipSpace
    ranges <- (textualDecoder @(Range a)) `Attoparsec.sepBy` (Attoparsec.char ',' >> Attoparsec.skipSpace)
    Attoparsec.skipSpace
    _ <- Attoparsec.char '}'
    pure (Multirange (Vector.fromList ranges))

instance (IsRangeElement a, Arbitrary a, Ord a) => Arbitrary (Multirange a) where
  arbitrary = do
    size <- QuickCheck.getSize
    QuickCheck.frequency
      [ ( 1,
          pure (Multirange Vector.empty)
        ),
        ( max 1 size,
          do
            lowerInfinity <- arbitrary
            upperInfinity <- arbitrary
            numRanges <- QuickCheck.chooseInt (0, max 0 size)
            let numBounds =
                  numRanges * 2 + bool 1 0 lowerInfinity + bool 1 0 upperInfinity
            bounds <- QuickCheckExtras.Gen.setOfSize numBounds (arbitrary @a)
            let preparedBounds =
                  mconcat
                    [ if lowerInfinity then [Nothing] else [],
                      fmap Just (Set.toList bounds),
                      if upperInfinity then [Nothing] else []
                    ]
                pairs =
                  BaseExtras.List.toPairs preparedBounds
                ranges =
                  fmap (onfrom . Just) pairs :: [Range a]

            pure
              (Multirange (Vector.fromList ranges))
        )
      ]

-- |
-- Direct conversion to and from Vector.
instance (Ord a) => IsSome (Vector (Range a)) (Multirange a) where
  to (Multirange ranges) = ranges

  maybeFrom unnormalized =
    let Multirange normalized = normalizeMultirange (Vector.toList unnormalized)
     in if unnormalized == normalized
          then Just (Multirange normalized)
          else Nothing

instance (Ord a) => IsMany (Vector (Range a)) (Multirange a) where
  onfrom = normalizeMultirange . Vector.toList

-- | Create a multirange from a list of ranges.
-- Performs the same normalization as PostgreSQL:
-- 1. Removes empty ranges
-- 2. Sorts ranges by their lower bounds
-- 3. Merges overlapping and adjacent ranges
normalizeMultirange :: (Ord a) => [Range a] -> Multirange a
normalizeMultirange = Multirange . Vector.fromList . mergeRanges . sortRanges . filterNonEmpty
  where
    -- Step 1: Remove empty ranges
    filterNonEmpty = filter (not . isEmptyRange)

    -- Step 2: Sort ranges by their lower bound
    sortRanges = sortBy compareRanges

    -- Step 3: Merge overlapping and adjacent ranges
    mergeRanges [] = []
    mergeRanges [r] = [r]
    mergeRanges (r1 : r2 : rs) =
      case mergeTwo r1 r2 of
        Just merged -> mergeRanges (merged : rs)
        Nothing -> r1 : mergeRanges (r2 : rs)

-- | Check if a range is empty
isEmptyRange :: (Ord a) => Range a -> Bool
isEmptyRange range = isNothing (rangeToBounds range)

-- | Convert range to bounds tuple
rangeToBounds :: (Ord a) => Range a -> Maybe (Maybe a, Maybe a)
rangeToBounds = to

-- | Convert bounds tuple to range
boundsToRange :: (Ord a) => Maybe (Maybe a, Maybe a) -> Maybe (Range a)
boundsToRange = maybeFrom

-- | Compare ranges by their lower bounds for sorting
compareRanges :: (Ord a) => Range a -> Range a -> Ordering
compareRanges r1 r2 =
  case (rangeToBounds r1, rangeToBounds r2) of
    (Nothing, Nothing) -> EQ -- Both empty
    (Nothing, Just _) -> LT -- Empty range comes first
    (Just _, Nothing) -> GT -- Empty range comes first
    (Just (lower1, _), Just (lower2, _)) -> compare lower1 lower2

-- | Merge two ranges if they are overlapping or adjacent
mergeTwo :: (Ord a) => Range a -> Range a -> Maybe (Range a)
mergeTwo r1 r2 =
  case (rangeToBounds r1, rangeToBounds r2) of
    (Nothing, _) -> Just r2 -- First range is empty, keep second
    (_, Nothing) -> Just r1 -- Second range is empty, keep first
    (Just (lower1, upper1), Just (lower2, upper2)) ->
      if areOverlappingOrAdjacent (lower1, upper1) (lower2, upper2)
        then boundsToRange (Just (minRangeBound lower1 lower2, maxRangeBound upper1 upper2))
        else Nothing

-- | Check if two range bounds are overlapping or adjacent
areOverlappingOrAdjacent :: (Ord a) => (Maybe a, Maybe a) -> (Maybe a, Maybe a) -> Bool
areOverlappingOrAdjacent (lower1, upper1) (lower2, upper2) =
  -- Ranges are overlapping or adjacent if the end of one is >= start of the other
  case (upper1, lower2) of
    (Nothing, _) -> True -- First range extends to infinity
    (_, Nothing) -> True -- Second range starts from negative infinity
    (Just u1, Just l2) ->
      u1 >= l2 && case (lower1, upper2) of
        (Nothing, _) -> True -- First range starts from negative infinity
        (_, Nothing) -> True -- Second range extends to infinity
        (Just l1, Just u2) -> l1 <= u2

-- | Get the minimum of two bounds (Nothing represents infinity)
minRangeBound :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minRangeBound Nothing _ = Nothing
minRangeBound _ Nothing = Nothing
minRangeBound (Just a) (Just b) = Just (min a b)

-- | Get the maximum of two bounds (Nothing represents infinity)
maxRangeBound :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxRangeBound Nothing _ = Nothing
maxRangeBound _ Nothing = Nothing
maxRangeBound (Just a) (Just b) = Just (max a b)
