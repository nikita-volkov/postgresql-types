module PostgresqlTypes.Timestamptz
  ( Timestamptz,

    -- * Accessors
    toMicroseconds,
    toUtcTime,

    -- * Constructors
    normalizeFromMicroseconds,
    normalizeFromUtcTime,
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

-- | PostgreSQL @timestamptz@ type. Date and time with time zone.
--
-- Gets stored as microseconds since PostgreSQL epoch, implying UTC timezone.
--
-- Range: @4713 BC@ to @294276 AD@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-TIMEZONES).
newtype Timestamptz = Timestamptz Int64
  deriving newtype (Eq, Ord, Hashable)
  deriving (Show, Read, IsString) via (ViaIsScalar Timestamptz)

instance Arbitrary Timestamptz where
  arbitrary = Timestamptz <$> QuickCheck.choose (minMicroseconds, maxMicroseconds)

instance IsScalar Timestamptz where
  schemaName = Tagged Nothing
  typeName = Tagged "timestamptz"
  baseOid = Tagged (Just 1184)
  arrayOid = Tagged (Just 1185)
  typeParams = Tagged []
  binaryEncoder (Timestamptz micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PtrPeeker.fixed PtrPeeker.beSignedInt8
    pure (Right (Timestamptz microseconds))
  textualEncoder (toUtcTime -> utcTime) =
    formatTimestamptzForPostgreSQL utcTime
  textualDecoder = do
    -- Parse date part
    y <- Attoparsec.decimal
    _ <- Attoparsec.char '-'
    m <- twoDigits
    _ <- Attoparsec.char '-'
    d <- twoDigits
    -- Space or T separator
    _ <- Attoparsec.satisfy (\c -> c == ' ' || c == 'T')
    -- Parse time part
    h <- twoDigits
    _ <- Attoparsec.char ':'
    mi <- twoDigits
    _ <- Attoparsec.char ':'
    s <- twoDigits
    micros <- Attoparsec.option 0 parseFraction
    -- Parse timezone offset
    tzOffsetMinutes <- parseTimezone
    -- Check for BC suffix
    bc <- Attoparsec.option False (True <$ (Attoparsec.skipSpace *> Attoparsec.string "BC"))
    let year = if bc then negate y + 1 else y
    case Time.fromGregorianValid year m d of
      Just day -> do
        let timeOfDay = Time.TimeOfDay h mi (fromIntegral s + fromIntegral micros / 1_000_000)
            localTime = Time.LocalTime day timeOfDay
            timeZone = Time.minutesToTimeZone tzOffsetMinutes
            utcTime = Time.localTimeToUTC timeZone localTime
        pure (normalizeFromUtcTime utcTime)
      Nothing ->
        -- For extreme dates, compute directly from PostgreSQL epoch
        let yearsSinceEpoch = year - 2000
            daysFromYears = fromIntegral yearsSinceEpoch * 365 + fromIntegral (yearsSinceEpoch `div` 4)
            getDaysInMonth mon =
              case mon of
                1 -> (31 :: Int)
                2 -> if isLeapYear year then (29 :: Int) else (28 :: Int)
                3 -> (31 :: Int)
                4 -> (30 :: Int)
                5 -> (31 :: Int)
                6 -> (30 :: Int)
                7 -> (31 :: Int)
                8 -> (31 :: Int)
                9 -> (30 :: Int)
                10 -> (31 :: Int)
                11 -> (30 :: Int)
                12 -> (31 :: Int)
                _ -> (0 :: Int)
            monthDays = foldl' (+) 0 [if mon < m then getDaysInMonth mon else 0 | mon <- [1 .. 12]]
            totalDays = daysFromYears + fromIntegral monthDays + fromIntegral d - 1
            dayMicros = totalDays * 86400_000_000
            timeMicros = fromIntegral h * 3600_000_000 + fromIntegral mi * 60_000_000 + fromIntegral s * 1_000_000 + fromIntegral micros
            tzAdjustment = fromIntegral tzOffsetMinutes * (-60_000_000) -- Subtract offset to get UTC
         in pure (Timestamptz (dayMicros + timeMicros + tzAdjustment))
    where
      twoDigits = do
        a <- Attoparsec.digit
        b <- Attoparsec.digit
        pure (digitToInt a * 10 + digitToInt b)
      parseFraction = do
        _ <- Attoparsec.char '.'
        digits <- Attoparsec.takeWhile1 isDigit
        let paddedDigits = take 6 (Text.unpack digits ++ repeat '0')
            micros = foldl' (\acc c -> acc * 10 + digitToInt c) 0 paddedDigits
        pure micros
      parseTimezone =
        (0 <$ Attoparsec.char 'Z')
          <|> do
            sign <- (1 <$ Attoparsec.char '+') <|> ((-1) <$ Attoparsec.char '-')
            h <- twoDigits
            mi <- Attoparsec.option 0 (Attoparsec.option ':' (Attoparsec.char ':') *> twoDigits)
            pure (sign * (h * 60 + mi))
      isLeapYear yr = (yr `mod` 4 == 0 && yr `mod` 100 /= 0) || (yr `mod` 400 == 0)

-- | Mapping to @tstzrange@ type.
instance IsRangeElement Timestamptz where
  rangeTypeName = Tagged "tstzrange"
  rangeBaseOid = Tagged (Just 3910)
  rangeArrayOid = Tagged (Just 3911)

-- | Mapping to @tstzmultirange@ type.
instance IsMultirangeElement Timestamptz where
  multirangeTypeName = Tagged "tstzmultirange"
  multirangeBaseOid = Tagged (Just 4534)
  multirangeArrayOid = Tagged (Just 6153)

-- * Accessors

-- | Convert PostgreSQL 'Timestamptz' to microseconds since epoch.
toMicroseconds :: Timestamptz -> Int64
toMicroseconds (Timestamptz micros) = micros

-- | Convert PostgreSQL 'Timestamptz' to 'Time.UTCTime'.
toUtcTime :: Timestamptz -> Time.UTCTime
toUtcTime (Timestamptz micros) =
  let diffTime = fromIntegral micros / 1_000_000
   in Time.addUTCTime diffTime postgresUtcEpoch

-- * Constructors

-- | Construct a PostgreSQL 'Timestamptz' from microseconds since epoch by clamping the values out of range.
normalizeFromMicroseconds :: Int64 -> Timestamptz
normalizeFromMicroseconds micros =
  if micros < minMicroseconds
    then Timestamptz minMicroseconds
    else
      if micros > maxMicroseconds
        then Timestamptz maxMicroseconds
        else Timestamptz micros

-- | Construct a PostgreSQL 'Timestamptz' from 'Time.UTCTime'.
normalizeFromUtcTime :: Time.UTCTime -> Timestamptz
normalizeFromUtcTime utcTime =
  let diffTime = Time.diffUTCTime utcTime postgresUtcEpoch
      micros = round (diffTime * 1_000_000)
   in normalizeFromMicroseconds micros

-- | Format a UTCTime for PostgreSQL timestamptz text format.
-- PostgreSQL requires specific formatting for extreme dates:
-- - Years must be 4-digit zero-padded for AD dates
-- - BC dates use "YYYY-MM-DD HH:MM:SS+0000 BC" format
-- - Always includes +0000 timezone for UTC
-- - Microseconds must be properly formatted
formatTimestamptzForPostgreSQL :: Time.UTCTime -> TextBuilder
formatTimestamptzForPostgreSQL utcTime =
  let day = Time.utctDay utcTime
      diffTime = Time.utctDayTime utcTime
      (year, month, dayOfMonth) = Time.toGregorian day
      -- Convert DiffTime to hours, minutes, seconds, microseconds
      totalSecs = floor (toRational diffTime) :: Integer
      totalMicros = round (toRational diffTime * 1000000) :: Integer
      micros = totalMicros `mod` 1000000 :: Integer
      secs = totalSecs `mod` 60 :: Integer
      totalMins = totalSecs `div` 60 :: Integer
      mins = totalMins `mod` 60 :: Integer
      hours = totalMins `div` 60 :: Integer
      timeBuilder =
        if micros == 0
          then
            mconcat
              [ TextBuilder.fixedLengthDecimal 2 (hours :: Integer),
                ":",
                TextBuilder.fixedLengthDecimal 2 (mins :: Integer),
                ":",
                TextBuilder.fixedLengthDecimal 2 (secs :: Integer)
              ]
          else
            mconcat
              [ TextBuilder.fixedLengthDecimal 2 (hours :: Integer),
                ":",
                TextBuilder.fixedLengthDecimal 2 (mins :: Integer),
                ":",
                TextBuilder.fixedLengthDecimal 2 (secs :: Integer),
                ".",
                TextBuilder.fixedLengthDecimal 6 (micros :: Integer)
              ]
   in if year <= 0
        then
          -- For BC dates (year <= 0), PostgreSQL expects format like "0001-01-01 00:00:00+0000 BC"
          -- Note: year 0 is 1 BC, year -1 is 2 BC, etc.
          let bcYear = negate (1 - year)
           in mconcat
                [ if bcYear <= 999
                    then TextBuilder.fixedLengthDecimal 4 (bcYear :: Integer)
                    else TextBuilder.decimal (bcYear :: Integer),
                  "-",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral month :: Integer),
                  "-",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral dayOfMonth :: Integer),
                  " ",
                  timeBuilder,
                  "+0000 BC"
                ]
        else
          -- For AD dates (year > 0), use zero-padded 4-digit year
          mconcat
            [ if year <= 999
                then TextBuilder.fixedLengthDecimal 4 (year :: Integer)
                else TextBuilder.decimal (year :: Integer),
              "-",
              TextBuilder.fixedLengthDecimal 2 (fromIntegral month :: Integer),
              "-",
              TextBuilder.fixedLengthDecimal 2 (fromIntegral dayOfMonth :: Integer),
              " ",
              timeBuilder,
              "+0000"
            ]

-- * Constants

-- PostgreSQL timestamptz epoch is 2000-01-01 00:00:00 UTC
postgresUtcEpoch :: Time.UTCTime
postgresUtcEpoch = Time.UTCTime (Time.fromGregorian 2000 1 1) 0

-- PostgreSQL's actual documented timestamptz range: 4713 BC to 294276 AD
-- Do not artificially restrict to avoid edge cases - let the tests expose real issues
minMicroseconds :: Int64
minMicroseconds = -210866803200000000 -- 4713 BC January 1 00:00:00 UTC

maxMicroseconds :: Int64
maxMicroseconds = 9214646400000000000 -- 294276 AD December 31 23:59:59.999999 UTC
