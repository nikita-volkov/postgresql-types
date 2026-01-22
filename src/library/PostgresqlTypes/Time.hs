module PostgresqlTypes.Time
  ( Time,

    -- * Accessors
    toMicroseconds,
    toTimeOfDay,

    -- * Constructors
    normalizeFromMicroseconds,
    refineFromTimeOfDay,
    normalizeFromTimeOfDay,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Time as Time
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @time@ type. Time of day (without time zone).
--
-- Gets stored as microseconds since midnight (@00:00:00@).
--
-- Range: @00:00:00@ to @24:00:00@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-TIME).
newtype Time = Time Int64
  deriving newtype (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Time)

instance Arbitrary Time where
  arbitrary = Time <$> QuickCheck.choose (toMicroseconds minBound, toMicroseconds maxBound)

instance IsScalar Time where
  schemaName = Tagged Nothing
  typeName = Tagged "time"
  baseOid = Tagged (Just 1083)
  arrayOid = Tagged (Just 1183)
  typeParams = Tagged []
  binaryEncoder (Time microseconds) = Write.bInt64 microseconds
  binaryDecoder = PtrPeeker.fixed (Right . Time <$> PtrPeeker.beSignedInt8)
  textualEncoder (Time microseconds) =
    let diffTime = fromIntegral microseconds / 1_000_000
        timeOfDay = Time.timeToTimeOfDay diffTime
     in TextBuilder.string (Time.formatTime Time.defaultTimeLocale "%H:%M:%S%Q" timeOfDay)
  textualDecoder = do
    h <- twoDigits
    _ <- Attoparsec.char ':'
    m <- twoDigits
    -- Seconds are optional
    s <- Attoparsec.option 0 (Attoparsec.char ':' *> parseSeconds)
    if h < 25 && m < 60 && s < 61_000_000
      then
        let microseconds = fromIntegral h * 3600_000_000 + fromIntegral m * 60_000_000 + s
         in pure (Time microseconds)
      else fail "Invalid time"
    where
      twoDigits = do
        a <- Attoparsec.digit
        b <- Attoparsec.digit
        pure (digitToInt a * 10 + digitToInt b)
      parseSeconds = do
        secs <- twoDigits
        micros <- Attoparsec.option 0 parseFraction
        pure (fromIntegral secs * 1_000_000 + micros)
      parseFraction = do
        _ <- Attoparsec.char '.'
        digits <- Attoparsec.takeWhile1 isDigit
        -- Pad or truncate to 6 digits for microseconds
        let paddedDigits = take 6 (Text.unpack digits ++ repeat '0')
            micros = foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) 0 paddedDigits
        pure micros

instance Bounded Time where
  minBound = Time 0
  maxBound = Time (24 * 60 * 60 * 1_000_000) -- 24 hours in microseconds

-- * Accessors

-- | Extract the underlying microseconds value.
toMicroseconds :: Time -> Int64
toMicroseconds (Time microseconds) = microseconds

-- | Convert PostgreSQL 'Time' to 'Time.TimeOfDay'.
toTimeOfDay :: Time -> Time.TimeOfDay
toTimeOfDay (Time microseconds) =
  let diffTime = fromIntegral microseconds / 1_000_000
   in Time.timeToTimeOfDay diffTime

-- * Constructors

-- | Construct a PostgreSQL 'Time' from microseconds, clamping to valid range.
normalizeFromMicroseconds :: Int64 -> Time
normalizeFromMicroseconds microseconds =
  if microseconds < 0
    then Time 0
    else
      if microseconds > 86_400_000_000
        then Time 86_400_000_000
        else Time microseconds

-- | Convert from 'Time.TimeOfDay' to PostgreSQL 'Time' with validation.
-- Returns 'Nothing' if the value is outside the valid range.
refineFromTimeOfDay :: Time.TimeOfDay -> Maybe Time
refineFromTimeOfDay timeOfDay =
  let diffTime = Time.timeOfDayToTime timeOfDay
      microseconds = round (diffTime * 1_000_000)
      time = Time microseconds
   in if microseconds >= 0 && microseconds <= 86_400_000_000 && toTimeOfDay time == timeOfDay
        then Just time
        else Nothing

-- | Convert from 'Time.TimeOfDay' to PostgreSQL 'Time', clamping to valid range.
normalizeFromTimeOfDay :: Time.TimeOfDay -> Time
normalizeFromTimeOfDay timeOfDay =
  let diffTime = Time.timeOfDayToTime timeOfDay
      microseconds = round (diffTime * 1_000_000)
   in normalizeFromMicroseconds microseconds
