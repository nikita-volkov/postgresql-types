module PostgresqlTypes.Types.Time (Time) where

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
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-TIME).
newtype Time = Time Int64
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Time)

instance Arbitrary Time where
  arbitrary = Time <$> QuickCheck.choose (toMicroseconds minBound, toMicroseconds maxBound)

instance IsStandardType Time where
  typeName = Tagged "time"
  baseOid = Tagged (Just 1083)
  arrayOid = Tagged (Just 1183)
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

-- | Convert from TimeOfDay to Time (microseconds)
instance IsSome Time.TimeOfDay Time where
  to (Time microseconds) =
    let diffTime = fromIntegral microseconds / 1_000_000
     in Time.timeToTimeOfDay diffTime
  maybeFrom timeOfDay =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
        time = Time microseconds
     in if microseconds >= 0 && microseconds <= 86_400_000_000 && to time == timeOfDay
          then Just time
          else Nothing

-- | Convert from TimeOfDay to Time, wrapping negative values around 24-hour period
instance IsMany Time.TimeOfDay Time where
  onfrom timeOfDay =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
        -- Wrap around 24-hour period for negative values
        wrappedMicroseconds = microseconds `mod` 86_400_000_000
     in Time wrappedMicroseconds

toMicroseconds :: Time -> Int64
toMicroseconds (Time microseconds) = microseconds
