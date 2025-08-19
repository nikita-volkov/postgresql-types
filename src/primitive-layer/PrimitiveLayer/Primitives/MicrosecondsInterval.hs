module PrimitiveLayer.Primitives.MicrosecondsInterval (MicrosecondsInterval) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (toInteger)
import PrimitiveLayer.Primitives.Interval (Interval)
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- |
-- PostgreSQL @interval@ type normalized to amount of microseconds.
--
-- The standard PostgreSQL @interval@ representation ('Interval') has separate amounts of months, days and microseconds with all having individual signs.
-- This one simplifies the representation to a single value in microseconds.
-- Thus, it can be easily compared, manipulated and converted to various other representations like 'Data.Time.DiffTime'.
newtype MicrosecondsInterval = MicrosecondsInterval Integer
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive MicrosecondsInterval)

instance Bounded MicrosecondsInterval where
  minBound = fromInterval minBound
  maxBound = fromInterval maxBound

instance Arbitrary MicrosecondsInterval where
  arbitrary =
    MicrosecondsInterval
      <$> QuickCheck.choose (toInteger minBound, toInteger maxBound)
  shrink (MicrosecondsInterval microseconds) =
    [MicrosecondsInterval microseconds' | microseconds' <- shrink microseconds]

instance Primitive MicrosecondsInterval where
  typeName = retag @Interval typeName
  baseOid = retag @Interval baseOid
  arrayOid = retag @Interval arrayOid
  binaryEncoder = binaryEncoder . toInterval
  binaryDecoder = fmap (fmap fromInterval) binaryDecoder
  textualEncoder = textualEncoder . toInterval

instance IsSome Interval MicrosecondsInterval where
  to = toInterval
  maybeFrom = Just . fromInterval

instance IsMany Interval MicrosecondsInterval where
  from = fromInterval

instance IsSome DiffTime MicrosecondsInterval where
  to = Time.picosecondsToDiffTime . (1_000_000 *) . toInteger
  maybeFrom = compileFromDiffTime

instance IsMany DiffTime MicrosecondsInterval where
  from = normalizeFromDiffTime

-- * Constants

microsPerDay :: (Num a) => a
microsPerDay = 10 ^ 6 * 60 * 60 * 24

daysPerMonth :: (Num a) => a
daysPerMonth = 30

-- * Conversion

toInteger :: MicrosecondsInterval -> Integer
toInteger (MicrosecondsInterval microseconds) = microseconds

toInterval :: MicrosecondsInterval -> Interval
toInterval (MicrosecondsInterval microseconds) =
  flip evalState microseconds do
    microseconds <- fromIntegral <$> state (swap . flip divMod microsPerDay)
    days <- fromIntegral <$> state (swap . flip divMod daysPerMonth)
    months <- fromIntegral <$> get
    pure (from @(Int32, Int32, Int64) (months, days, microseconds))

fromInterval :: Interval -> MicrosecondsInterval
fromInterval interval =
  let (months, days, microseconds) = to @(Int32, Int32, Int64) interval
      totalDays = fromIntegral days + daysPerMonth * fromIntegral months
      totalMicros = fromIntegral microseconds + microsPerDay * totalDays
   in MicrosecondsInterval totalMicros

-- * Compilation

-- | Performs bounds check.
compileFromMicroseconds :: Integer -> Maybe MicrosecondsInterval
compileFromMicroseconds microseconds =
  let wrapped = MicrosecondsInterval microseconds
   in if wrapped >= minBound && wrapped <= maxBound
        then Just wrapped
        else Nothing

-- | Performs precision loss check.
compileFromPicoseconds :: Integer -> Maybe MicrosecondsInterval
compileFromPicoseconds picoseconds =
  let (microseconds, remainder) = divMod picoseconds 1_000_000
   in if remainder == 0
        then compileFromMicroseconds microseconds
        else Nothing

compileFromDiffTime :: DiffTime -> Maybe MicrosecondsInterval
compileFromDiffTime = compileFromPicoseconds . Time.diffTimeToPicoseconds

-- * Normalization

-- | Clamps to bounds.
normalizeFromMicroseconds :: Integer -> MicrosecondsInterval
normalizeFromMicroseconds microseconds =
  let wrapped = MicrosecondsInterval microseconds
   in if wrapped < minBound
        then minBound
        else
          if wrapped > maxBound
            then maxBound
            else wrapped

-- | Lose the submicrosecond precision.
normalizeFromPicoseconds :: Integer -> MicrosecondsInterval
normalizeFromPicoseconds picoseconds =
  let microseconds = div picoseconds 1_000_000
   in normalizeFromMicroseconds microseconds

normalizeFromDiffTime :: DiffTime -> MicrosecondsInterval
normalizeFromDiffTime = normalizeFromPicoseconds . Time.diffTimeToPicoseconds
