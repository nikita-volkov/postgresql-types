module PostgresqlTypes.Multirange
  ( Multirange,

    -- * Accessors
    toRangeList,
    toRangeVector,

    -- * Constructors
    normalizeFromRangeList,
    refineFromRangeList,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import PostgresqlTypes.Algebra
import qualified PostgresqlTypes.Multirange.List
import qualified PostgresqlTypes.Multirange.QuickCheckGen as QuickCheckGen
import PostgresqlTypes.Prelude
import PostgresqlTypes.Range (Range)
import qualified PostgresqlTypes.Range as Range
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
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
  deriving (Show, Read, IsString) via (ViaIsScalar (Multirange a))

instance (IsMultirangeElement a) => IsScalar (Multirange a) where
  schemaName = Tagged Nothing
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
        let write = binaryEncoder range
         in Write.bWord32 (fromIntegral (Write.writeSize write)) <> write

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
    ranges <- (textualDecoder @(Range a)) `Attoparsec.sepBy` (Attoparsec.skipSpace >> Attoparsec.char ',' >> Attoparsec.skipSpace)
    Attoparsec.skipSpace
    _ <- Attoparsec.char '}'
    Attoparsec.skipSpace
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
            bounds <- QuickCheckGen.setOfSize numBounds (arbitrary @a)
            let preparedBounds =
                  mconcat
                    [ if lowerInfinity then [Nothing] else [],
                      fmap Just (Set.toList bounds),
                      if upperInfinity then [Nothing] else []
                    ]
                pairs =
                  PostgresqlTypes.Multirange.List.toPairs preparedBounds
                ranges =
                  fmap (uncurry Range.normalizeBounded) pairs :: [Range a]

            pure
              (Multirange (Vector.fromList ranges))
        )
      ]

instance (Hashable a) => Hashable (Multirange a) where
  hashWithSalt salt (Multirange ranges) = hashWithSalt salt (Vector.toList ranges)

-- | Create a list of ranges from a multirange.
toRangeList :: Multirange a -> [Range a]
toRangeList (Multirange ranges) = Vector.toList ranges

-- | Create a vector of ranges from a multirange.
toRangeVector :: Multirange a -> Vector (Range a)
toRangeVector (Multirange ranges) = ranges

-- | Create a multirange from a list of ranges.
-- Performs the same normalization as PostgreSQL:
-- 1. Removes empty ranges
-- 2. Sorts ranges by their lower bounds
-- 3. Merges overlapping and adjacent ranges
normalizeFromRangeList :: (Ord a) => [Range a] -> Multirange a
normalizeFromRangeList = Multirange . Vector.fromList . mergeRanges . sortRanges . filterNonEmpty
  where
    -- Step 1: Remove empty ranges
    filterNonEmpty = filter (not . Range.isEmpty)

    -- Step 2: Sort ranges by their lower bound
    sortRanges = sort

    -- Step 3: Merge overlapping and adjacent ranges
    mergeRanges [] = []
    mergeRanges [r] = [r]
    mergeRanges (r1 : r2 : rs) =
      case Range.mergeIfOverlappingOrAdjacent r1 r2 of
        Just merged -> mergeRanges (merged : rs)
        Nothing -> r1 : mergeRanges (r2 : rs)

-- | Attempt to create a multirange from a list of ranges.
-- Returns 'Nothing' if the input list is not already normalized.
refineFromRangeList :: (Ord a) => [Range a] -> Maybe (Multirange a)
refineFromRangeList ranges =
  -- Check if the input is already normalized by comparing against the normalized version.
  -- A more efficient implementation would check properties directly:
  -- 1. No empty ranges
  -- 2. Ranges are sorted
  -- 3. No adjacent or overlapping ranges
  -- However, the current approach is simpler and correct.
  let Multirange normalized = normalizeFromRangeList ranges
      unnormalized = Vector.fromList ranges
   in if unnormalized == normalized
        then Just (Multirange normalized)
        else Nothing
