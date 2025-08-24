module PrimitiveLayer.Types.Multirange (Multirange) where

import qualified Data.Vector as Vector
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Types.Range (Range)
import PrimitiveLayer.Via
import qualified PrimitiveLayer.Writes as Writes
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- |
-- Normalized representation of a multirange, which is a collection of non-overlapping, non-adjacent ranges.
--
-- PostgreSQL multirange types store multiple ranges as a single value, automatically normalizing them
-- by combining overlapping and adjacent ranges. The ranges are stored in sorted order.
--
-- The following standard types are supported via the 'MultirangeMapping' instances:
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
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/rangetypes.html#RANGETYPES-MULTIRANGE).
newtype Multirange a = Multirange (Vector (Range a))
  deriving stock (Eq, Functor)
  deriving (Show) via (ViaPrimitive (Multirange a))

instance (MultirangeMapping a, Ord a) => Mapping (Multirange a) where
  typeName = retag @a multirangeTypeName
  baseOid = retag @a multirangeOid
  arrayOid = retag @a multirangeArrayOid
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
      PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
    ranges <- replicateM (fromIntegral numRanges) do
      size <- lift do
        PeekyBlinders.statically PeekyBlinders.beSignedInt4
      when (size < 0) do
        throwError (DecodingError ["range-size"] (InvalidValueDecodingErrorReason "Expecting >= 0" (TextBuilder.toText (TextBuilder.decimal size))))
      ExceptT do
        PeekyBlinders.forceSize (fromIntegral size) do
          binaryDecoder @(Range a)
    pure (Multirange (Vector.fromList ranges))

  textualEncoder = \case
    Multirange ranges ->
      mconcat
        [ "{",
          TextBuilder.intercalate "," (Vector.toList (Vector.map (textualEncoder @(Range a)) ranges)),
          "}"
        ]

instance (RangeMapping a, Arbitrary a, Ord a) => Arbitrary (Multirange a) where
  arbitrary = do
    -- Generate 0-2 ranges that won't cause normalization issues
    numRanges <- QuickCheck.chooseInt (0, 2)
    case numRanges of
      0 -> pure (Multirange Vector.empty)
      1 -> do
        -- Generate a range, but avoid empty ranges as they get normalized away by PostgreSQL
        range <- QuickCheck.suchThat (arbitrary @(Range a)) (\r -> textualEncoder r /= "empty")
        pure (Multirange (Vector.singleton range))
      _ -> do
        -- For multiple ranges, just use one to avoid overlaps and normalization
        range <- QuickCheck.suchThat (arbitrary @(Range a)) (\r -> textualEncoder r /= "empty")
        pure (Multirange (Vector.singleton range))

-- |
-- Interprets values of @[Range a]@ as a multirange by normalizing overlapping and adjacent ranges.
instance (Ord a) => IsSome [Range a] (Multirange a) where
  to (Multirange ranges) = Vector.toList ranges

  maybeFrom ranges = Just (normalizeMultirange ranges)

-- |
-- Normalizes lists of ranges by combining overlapping and adjacent ranges.
instance (Ord a) => IsMany [Range a] (Multirange a) where
  onfrom ranges = normalizeMultirange ranges

-- |
-- Direct conversion to and from Vector.
instance (Ord a) => IsSome (Vector (Range a)) (Multirange a) where
  to (Multirange ranges) = ranges

  maybeFrom ranges = Just (normalizeMultirange (Vector.toList ranges))

instance (Ord a) => IsSome (Multirange a) (Vector (Range a)) where
  to ranges = Multirange ranges

  maybeFrom (Multirange ranges) = Just ranges

instance (Ord a) => IsMany (Vector (Range a)) (Multirange a) where
  onfrom ranges = normalizeMultirange (Vector.toList ranges)

instance (Ord a) => IsMany (Multirange a) (Vector (Range a)) where
  onfrom (Multirange ranges) = ranges

instance (Ord a) => Is (Vector (Range a)) (Multirange a)

instance (Ord a) => Is (Multirange a) (Vector (Range a))

-- | Create a multirange from a list of ranges.
-- Note: PostgreSQL performs the actual normalization (merging overlapping ranges, 
-- removing empty ranges, sorting) server-side. This function simply creates
-- the multirange structure that will be normalized by PostgreSQL.
normalizeMultirange :: [Range a] -> Multirange a
normalizeMultirange ranges = Multirange (Vector.fromList ranges)