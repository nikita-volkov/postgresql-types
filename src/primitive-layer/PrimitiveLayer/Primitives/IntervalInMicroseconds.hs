module PrimitiveLayer.Primitives.IntervalInMicroseconds (IntervalInMicroseconds) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (toInteger)
import PrimitiveLayer.Primitives.Interval (Interval)
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- |
-- PostgreSQL @interval@ type normalized to amount of microseconds.
--
-- The standard PostgreSQL @interval@ representation ('Interval') has separate amounts of months, days and microseconds with all having individual signs.
-- This one simplifies the representation to a single value in microseconds.
-- Thus, it can be easily compared, manipulated and converted to various other representations like 'Data.Time.DiffTime'.
newtype IntervalInMicroseconds = IntervalInMicroseconds Integer
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive IntervalInMicroseconds)
  deriving (Primitive) via (ViaIsMany Interval IntervalInMicroseconds)

instance Bounded IntervalInMicroseconds where
  minBound = fromInterval minBound
  maxBound = fromInterval maxBound

instance Arbitrary IntervalInMicroseconds where
  arbitrary =
    IntervalInMicroseconds
      <$> QuickCheck.choose (toInteger minBound, toInteger maxBound)
  shrink (IntervalInMicroseconds microseconds) =
    [IntervalInMicroseconds microseconds' | microseconds' <- shrink microseconds]

instance IsSome Interval IntervalInMicroseconds where
  to = toInterval
  maybeFrom = Just . fromInterval

instance IsMany Interval IntervalInMicroseconds where
  from = fromInterval

instance IsSome DiffTime IntervalInMicroseconds where
  to = Time.picosecondsToDiffTime . (1_000_000 *) . toInteger
  maybeFrom = compileFromDiffTime

instance IsMany DiffTime IntervalInMicroseconds where
  from = normalizeFromDiffTime

-- * Constants

microsPerDay :: (Num a) => a
microsPerDay = 10 ^ 6 * 60 * 60 * 24

daysPerMonth :: (Num a) => a
daysPerMonth = 30

-- * Conversion

toInteger :: IntervalInMicroseconds -> Integer
toInteger (IntervalInMicroseconds microseconds) = microseconds

toInterval :: IntervalInMicroseconds -> Interval
toInterval (IntervalInMicroseconds microseconds) =
  flip evalState microseconds do
    microseconds <- fromIntegral <$> state (swap . flip divMod microsPerDay)
    days <- fromIntegral <$> state (swap . flip divMod daysPerMonth)
    months <- fromIntegral <$> get
    pure (from @(Int32, Int32, Int64) (months, days, microseconds))

fromInterval :: Interval -> IntervalInMicroseconds
fromInterval interval =
  let (months, days, microseconds) = to @(Int32, Int32, Int64) interval
      totalDays = fromIntegral days + daysPerMonth * fromIntegral months
      totalMicros = fromIntegral microseconds + microsPerDay * totalDays
   in IntervalInMicroseconds totalMicros

-- * Compilation

-- | Performs bounds check.
compileFromMicroseconds :: Integer -> Maybe IntervalInMicroseconds
compileFromMicroseconds microseconds =
  let wrapped = IntervalInMicroseconds microseconds
   in if wrapped >= minBound && wrapped <= maxBound
        then Just wrapped
        else Nothing

-- | Performs precision loss check.
compileFromPicoseconds :: Integer -> Maybe IntervalInMicroseconds
compileFromPicoseconds picoseconds =
  let (microseconds, remainder) = divMod picoseconds 1_000_000
   in if remainder == 0
        then compileFromMicroseconds microseconds
        else Nothing

compileFromDiffTime :: DiffTime -> Maybe IntervalInMicroseconds
compileFromDiffTime = compileFromPicoseconds . Time.diffTimeToPicoseconds

-- * Normalization

-- | Clamps to bounds.
normalizeFromMicroseconds :: Integer -> IntervalInMicroseconds
normalizeFromMicroseconds microseconds =
  let wrapped = IntervalInMicroseconds microseconds
   in if wrapped < minBound
        then minBound
        else
          if wrapped > maxBound
            then maxBound
            else wrapped

-- | Lose the submicrosecond precision.
normalizeFromPicoseconds :: Integer -> IntervalInMicroseconds
normalizeFromPicoseconds picoseconds =
  let microseconds = div picoseconds 1_000_000
   in normalizeFromMicroseconds microseconds

normalizeFromDiffTime :: DiffTime -> IntervalInMicroseconds
normalizeFromDiffTime = normalizeFromPicoseconds . Time.diffTimeToPicoseconds
