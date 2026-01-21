module PostgresqlTypes.Types.IntervalAsMicroseconds (IntervalAsMicroseconds) where

import qualified Data.Time as Time
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (toInteger)
import PostgresqlTypes.Types.Interval (Interval)
import PostgresqlTypes.Via
import qualified Test.QuickCheck as QuickCheck

-- | PostgreSQL @interval@ type normalized to amount of microseconds.
--
-- The standard PostgreSQL @interval@ representation ('Interval') has separate amounts of months, days and microseconds with all having individual signs.
-- This one simplifies the representation to a single value in microseconds.
-- Thus, it can be easily compared, manipulated and converted to various other representations like 'Data.Time.DiffTime'.
--
-- Range: @-178000000@ years to @178000000@ years.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-INTERVAL-INPUT).
newtype IntervalAsMicroseconds = IntervalAsMicroseconds Integer
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar IntervalAsMicroseconds)
  deriving (IsScalar) via (ViaIsMany Interval IntervalAsMicroseconds)

instance Bounded IntervalAsMicroseconds where
  minBound = fromInterval minBound
  maxBound = fromInterval maxBound

instance Arbitrary IntervalAsMicroseconds where
  arbitrary =
    IntervalAsMicroseconds
      <$> QuickCheck.choose (toInteger minBound, toInteger maxBound)
  shrink (IntervalAsMicroseconds microseconds) =
    [IntervalAsMicroseconds microseconds' | microseconds' <- shrink microseconds]

instance IsSome Interval IntervalAsMicroseconds where
  to = toInterval
  maybeFrom interval =
    let intervalAsMicros = fromInterval interval
     in if to intervalAsMicros == interval
          then Just intervalAsMicros
          else Nothing

instance IsMany Interval IntervalAsMicroseconds where
  onfrom = fromInterval

instance IsSome DiffTime IntervalAsMicroseconds where
  to = Time.picosecondsToDiffTime . (1_000_000 *) . toInteger
  maybeFrom = refineFromDiffTime

instance IsMany DiffTime IntervalAsMicroseconds where
  onfrom = normalizeFromDiffTime

-- | Conversion to 'Integer' amount of microseconds.
instance IsSome Integer IntervalAsMicroseconds where
  to = toInteger
  maybeFrom = refineFromMicroseconds

-- | Conversion to 'Integer' amount of microseconds.
instance IsMany Integer IntervalAsMicroseconds where
  onfrom = normalizeFromMicroseconds

-- * Constants

microsPerDay :: (Num a) => a
microsPerDay = 1_000_000 * 60 * 60 * 24

daysPerMonth :: (Num a) => a
daysPerMonth = 30

-- * Conversion

toInteger :: IntervalAsMicroseconds -> Integer
toInteger (IntervalAsMicroseconds microseconds) = microseconds

toInterval :: IntervalAsMicroseconds -> Interval
toInterval (IntervalAsMicroseconds microseconds) =
  flip evalState microseconds do
    microseconds <- fromIntegral <$> state (swap . flip divMod microsPerDay)
    days <- fromIntegral <$> state (swap . flip divMod daysPerMonth)
    months <- fromIntegral <$> get
    pure (onfrom @(Int32, Int32, Int64) (months, days, microseconds))

fromInterval :: Interval -> IntervalAsMicroseconds
fromInterval interval =
  let (months, days, microseconds) = to @(Int32, Int32, Int64) interval
      totalDays = fromIntegral days + daysPerMonth * fromIntegral months
      totalMicros = fromIntegral microseconds + microsPerDay * totalDays
   in IntervalAsMicroseconds totalMicros

-- * Compilation

-- | Performs bounds check.
refineFromMicroseconds :: Integer -> Maybe IntervalAsMicroseconds
refineFromMicroseconds microseconds =
  let wrapped = IntervalAsMicroseconds microseconds
   in if wrapped >= minBound && wrapped <= maxBound
        then Just wrapped
        else Nothing

-- | Performs precision loss check.
refineFromPicoseconds :: Integer -> Maybe IntervalAsMicroseconds
refineFromPicoseconds picoseconds =
  let (microseconds, remainder) = divMod picoseconds 1_000_000
   in if remainder == 0
        then refineFromMicroseconds microseconds
        else Nothing

refineFromDiffTime :: DiffTime -> Maybe IntervalAsMicroseconds
refineFromDiffTime = refineFromPicoseconds . Time.diffTimeToPicoseconds

-- * Normalization

-- | Clamps to bounds.
normalizeFromMicroseconds :: Integer -> IntervalAsMicroseconds
normalizeFromMicroseconds microseconds =
  let wrapped = IntervalAsMicroseconds microseconds
   in if wrapped < minBound
        then minBound
        else
          if wrapped > maxBound
            then maxBound
            else wrapped

-- | Lose the submicrosecond precision.
normalizeFromPicoseconds :: Integer -> IntervalAsMicroseconds
normalizeFromPicoseconds picoseconds =
  let microseconds = div picoseconds 1_000_000
   in normalizeFromMicroseconds microseconds

normalizeFromDiffTime :: DiffTime -> IntervalAsMicroseconds
normalizeFromDiffTime = normalizeFromPicoseconds . Time.diffTimeToPicoseconds
