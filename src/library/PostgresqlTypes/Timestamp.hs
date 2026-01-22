module PostgresqlTypes.Timestamp
  ( Timestamp,

    -- * Accessors
    toMicroseconds,
    toLocalTime,

    -- * Constructors
    normalizeFromMicroseconds,
    normalizeFromLocalTime,
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

-- | PostgreSQL @timestamp@ type. Date and time (without time zone).
--
-- Gets stored as microseconds since PostgreSQL epoch.
--
-- Range: @4713 BC@ to @294276 AD@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-DATETIME).
newtype Timestamp = Timestamp Int64
  deriving newtype (Eq, Ord, Hashable)
  deriving (Show, Read, IsString) via (ViaIsScalar Timestamp)

instance Arbitrary Timestamp where
  arbitrary = Timestamp <$> QuickCheck.choose (minMicroseconds, maxMicroseconds)

instance IsScalar Timestamp where
  schemaName = Tagged Nothing
  typeName = Tagged "timestamp"
  baseOid = Tagged (Just 1114)
  arrayOid = Tagged (Just 1115)
  typeParams = Tagged []
  binaryEncoder (Timestamp micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PtrPeeker.fixed PtrPeeker.beSignedInt8
    pure (Right (Timestamp microseconds))
  textualEncoder (toLocalTime -> localTime) =
    formatTimestampForPostgreSQL localTime
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
    -- Check for BC suffix
    bc <- Attoparsec.option False (True <$ (Attoparsec.skipSpace *> Attoparsec.string "BC"))
    let year = if bc then negate y + 1 else y
    case Time.fromGregorianValid year m d of
      Just day -> do
        let timeOfDay = Time.TimeOfDay h mi (fromIntegral s + fromIntegral micros / 1_000_000)
            localTime = Time.LocalTime day timeOfDay
        pure (normalizeFromLocalTime localTime)
      Nothing ->
        -- For extreme dates, compute directly from PostgreSQL epoch
        -- This is a simplified calculation for extreme timestamps
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
         in pure (Timestamp (dayMicros + timeMicros))
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
      isLeapYear yr = (yr `mod` 4 == 0 && yr `mod` 100 /= 0) || (yr `mod` 400 == 0)

-- | Mapping to @tsrange@ type.
instance IsRangeElement Timestamp where
  rangeTypeName = Tagged "tsrange"
  rangeBaseOid = Tagged (Just 3908)
  rangeArrayOid = Tagged (Just 3909)

-- | Mapping to @tsmultirange@ type.
instance IsMultirangeElement Timestamp where
  multirangeTypeName = Tagged "tsmultirange"
  multirangeBaseOid = Tagged (Just 4533)
  multirangeArrayOid = Tagged (Just 6152)

-- * Accessors

-- | Convert PostgreSQL 'Timestamp' to microseconds since epoch.
toMicroseconds :: Timestamp -> Int64
toMicroseconds (Timestamp micros) = micros

-- | Convert PostgreSQL 'Timestamp' to 'Time.LocalTime'.
toLocalTime :: Timestamp -> Time.LocalTime
toLocalTime (Timestamp micros) =
  let diffTime = fromIntegral micros / 1_000_000
   in Time.addLocalTime diffTime postgresTimestampEpoch

-- * Constructors

-- | Construct a PostgreSQL 'Timestamp' from microseconds since epoch by clamping the values out of range.
normalizeFromMicroseconds :: Int64 -> Timestamp
normalizeFromMicroseconds micros =
  if micros < minMicroseconds
    then Timestamp minMicroseconds
    else
      if micros > maxMicroseconds
        then Timestamp maxMicroseconds
        else Timestamp micros

-- | Construct a PostgreSQL 'Timestamp' from 'Time.LocalTime'.
normalizeFromLocalTime :: Time.LocalTime -> Timestamp
normalizeFromLocalTime localTime =
  let diffTime = Time.diffLocalTime localTime postgresTimestampEpoch
      micros = round (diffTime * 1_000_000)
   in normalizeFromMicroseconds micros

-- | Format a LocalTime for PostgreSQL timestamp text format.
-- PostgreSQL requires specific formatting for extreme dates:
-- - Years must be 4-digit zero-padded for AD dates
-- - BC dates use "YYYY-MM-DD HH:MM:SS BC" format
-- - Microseconds must be properly formatted
formatTimestampForPostgreSQL :: Time.LocalTime -> TextBuilder
formatTimestampForPostgreSQL localTime =
  let day = Time.localDay localTime
      timeOfDay = Time.localTimeOfDay localTime
      (year, month, dayOfMonth) = Time.toGregorian day
      timeBuilder = formatTimeOfDay timeOfDay
   in if year <= 0
        then
          -- For BC dates (year <= 0), PostgreSQL expects format like "0001-01-01 00:00:00 BC"
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
                  " BC"
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
              timeBuilder
            ]
  where
    formatTimeOfDay :: Time.TimeOfDay -> TextBuilder
    formatTimeOfDay tod =
      let h = Time.todHour tod :: Int
          m = Time.todMin tod :: Int
          pico = Time.todSec tod
          -- Convert from Pico to microseconds
          totalMicros = round (toRational pico * 1000000) :: Integer
          secs = totalMicros `div` 1000000 :: Integer
          micros = totalMicros `mod` 1000000 :: Integer
       in if micros == 0
            then
              mconcat
                [ TextBuilder.fixedLengthDecimal 2 (fromIntegral h :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral m :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 secs
                ]
            else
              mconcat
                [ TextBuilder.fixedLengthDecimal 2 (fromIntegral h :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral m :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 secs,
                  ".",
                  TextBuilder.fixedLengthDecimal 6 micros
                ]

-- * Constants

-- PostgreSQL timestamp epoch is 2000-01-01 00:00:00
postgresTimestampEpoch :: Time.LocalTime
postgresTimestampEpoch = Time.LocalTime (Time.fromGregorian 2000 1 1) Time.midnight

-- PostgreSQL's actual documented timestamp range: 4713 BC to 294276 AD
-- Do not artificially restrict to avoid edge cases - let the tests expose real issues
minMicroseconds :: Int64
minMicroseconds = -210866803200000000 -- 4713 BC January 1 00:00:00

maxMicroseconds :: Int64
maxMicroseconds = 9214646400000000000 -- 294276 AD December 31 23:59:59.999999
