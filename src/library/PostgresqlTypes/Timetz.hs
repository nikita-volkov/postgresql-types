module PostgresqlTypes.Timetz
  ( Timetz,

    -- * Accessors
    toTimeInMicroseconds,
    toTimeZoneInSeconds,
    toTimeOfDay,
    normalizeToTimeZone,
    refineToTimeZone,

    -- * Constructors
    normalizeFromTimeInMicrosecondsAndOffsetInSeconds,
    normalizeFromTimeOfDayAndTimeZone,
    refineFromTimeInMicrosecondsAndOffsetInSeconds,
    refineFromTimeOfDayAndTimeZone,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Time as TimeLib
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import qualified PostgresqlTypes.Timetz.Offset as Offset
import qualified PostgresqlTypes.Timetz.Time as Time
import PostgresqlTypes.Via
import qualified PtrPeeker

-- | PostgreSQL @timetz@ type. Time of day with time zone.
--
-- Stored as microseconds since midnight and time zone offset in seconds.
--
-- Low value: @00:00:00+1559@. High value: @24:00:00-1559@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-TIMEZONES).
data Timetz
  = Timetz
      -- | Time as microseconds since midnight (00:00:00)
      Time.TimetzTime
      -- | Timezone offset in seconds (positive is east of UTC, negative is west of UTC)
      Offset.TimetzOffset
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Timetz)

instance Arbitrary Timetz where
  arbitrary = do
    time <- arbitrary
    offset <- arbitrary
    pure (Timetz time offset)

instance IsScalar Timetz where
  typeName = Tagged "timetz"
  baseOid = Tagged (Just 1266)
  arrayOid = Tagged (Just 1270)
  typeParams = Tagged []

  binaryEncoder (Timetz time offset) =
    mconcat
      [ Time.binaryEncoder time,
        Offset.binaryEncoder offset
      ]

  binaryDecoder =
    PtrPeeker.fixed do
      time <- Time.binaryDecoder
      offset <- Offset.binaryDecoder
      pure (Timetz <$> time <*> offset)

  -- Format:
  -- 23:59:59-15:59:59
  -- 24:00:00-15:59:59
  -- 00:00:00+15:59:59
  textualEncoder (Timetz time offset) =
    Time.renderInTextFormat time <> Offset.renderInTextFormat offset
  textualDecoder = do
    -- Parse time part: HH:MM:SS[.microseconds]
    h <- twoDigits
    _ <- Attoparsec.char ':'
    m <- twoDigits
    _ <- Attoparsec.char ':'
    s <- twoDigits
    micros <- Attoparsec.option 0 parseFraction
    -- Parse time zone offset: [+-]HH[:MM[:SS]]
    -- PostgreSQL omits minutes and seconds when they are zero
    sign <- (1 <$ Attoparsec.char '+') <|> ((-1) <$ Attoparsec.char '-')
    tzH <- twoDigits
    tzM <- Attoparsec.option 0 (Attoparsec.char ':' *> twoDigits)
    tzS <- Attoparsec.option 0 (Attoparsec.char ':' *> twoDigits)
    -- Build time and offset
    let timeMicros = fromIntegral h * 3600_000_000 + fromIntegral m * 60_000_000 + fromIntegral s * 1_000_000 + micros
        -- Note: PostgreSQL stores offset with inverted sign (positive means west of UTC)
        offsetSeconds = negate sign * (fromIntegral tzH * 3600 + fromIntegral tzM * 60 + fromIntegral tzS)
    case (Time.refineFromMicroseconds timeMicros, Offset.refineFromSeconds offsetSeconds) of
      (Just time, Just offset) -> pure (Timetz time offset)
      _ -> fail "Invalid timetz value"
    where
      twoDigits = do
        a <- Attoparsec.digit
        b <- Attoparsec.digit
        pure (digitToInt a * 10 + digitToInt b)
      parseFraction = do
        _ <- Attoparsec.char '.'
        digits <- Attoparsec.takeWhile1 isDigit
        let paddedDigits = take 6 (Text.unpack digits ++ repeat '0')
            micros = foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) 0 paddedDigits
        pure micros

-- * Accessors

-- | Extract time in microseconds since midnight.
toTimeInMicroseconds :: Timetz -> Int64
toTimeInMicroseconds (Timetz time _) = Time.toMicroseconds time

-- | Extract time zone offset in seconds.
toTimeZoneInSeconds :: Timetz -> Int32
toTimeZoneInSeconds (Timetz _ offset) = Offset.toSeconds offset

-- | Extract time of day.
toTimeOfDay :: Timetz -> TimeLib.TimeOfDay
toTimeOfDay (Timetz time _) = Time.toTimeOfDay time

-- | Extract time zone rounding the offset in seconds to the nearest minute, because that's the precision supported by 'TimeLib.TimeZone'.
normalizeToTimeZone :: Timetz -> TimeLib.TimeZone
normalizeToTimeZone (Timetz _ offset) = Offset.normalizeToTimeZone offset

-- | Try to extract time zone, failing if the offset in seconds is not a multiple of 60.
refineToTimeZone :: Timetz -> Maybe TimeLib.TimeZone
refineToTimeZone (Timetz _ offset) = Offset.refineToTimeZone offset

-- * Constructors

-- | Construct 'Timetz' from time in microseconds since midnight and time zone offset in seconds, clamping the out of range values.
normalizeFromTimeInMicrosecondsAndOffsetInSeconds :: Int64 -> Int32 -> Timetz
normalizeFromTimeInMicrosecondsAndOffsetInSeconds microseconds offset =
  Timetz (Time.normalizeFromMicroseconds microseconds) (Offset.normalizeFromSeconds offset)

-- | Construct 'Timetz' from 'TimeLib.TimeOfDay' and 'TimeLib.TimeZone', clamping the out of range values.
normalizeFromTimeOfDayAndTimeZone :: TimeLib.TimeOfDay -> TimeLib.TimeZone -> Timetz
normalizeFromTimeOfDayAndTimeZone timeOfDay timeZone =
  let time = Time.normalizeFromTimeOfDay timeOfDay
      offset = Offset.normalizeFromTimeZone timeZone
   in Timetz time offset

-- | Try to construct 'Timetz' from time in microseconds since midnight and time zone offset in seconds, failing if out of range.
refineFromTimeInMicrosecondsAndOffsetInSeconds :: Int64 -> Int32 -> Maybe Timetz
refineFromTimeInMicrosecondsAndOffsetInSeconds microseconds offset = do
  time <- Time.refineFromMicroseconds microseconds
  offset <- Offset.refineFromSeconds offset
  pure (Timetz time offset)

-- | Try to construct 'Timetz' from 'TimeLib.TimeOfDay' and 'TimeLib.TimeZone', failing if out of range.
refineFromTimeOfDayAndTimeZone :: TimeLib.TimeOfDay -> TimeLib.TimeZone -> Maybe Timetz
refineFromTimeOfDayAndTimeZone timeOfDay timeZone = do
  time <- Time.refineFromTimeOfDay timeOfDay
  offset <- Offset.refineFromTimeZone timeZone
  pure (Timetz time offset)
