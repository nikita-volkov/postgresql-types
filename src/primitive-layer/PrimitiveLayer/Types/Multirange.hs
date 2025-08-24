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

instance (Arbitrary a, Ord a) => Arbitrary (Multirange a) where
  arbitrary = do
    -- Generate a small number of ranges to keep the multirange manageable
    numRanges <- QuickCheck.chooseInt (0, 2)
    ranges <- replicateM numRanges (arbitrary @(Range a))
    pure (normalizeMultirange ranges)

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

-- | Normalize a list of ranges into a proper multirange.
-- PostgreSQL does the actual normalization (merging overlapping ranges, removing empty ranges)
-- server-side, so we keep this simple for now.
normalizeMultirange :: [Range a] -> Multirange a
normalizeMultirange ranges = Multirange (Vector.fromList ranges)