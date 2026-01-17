{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Interval (Interval) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @interval@ type. Time span with separate components for months, days, and microseconds with individual signs.
--
-- For a simpler and more portable representation consider 'PostgresqlTypes.Types.IntervalAsMicroseconds.IntervalAsMicroseconds'.
--
-- Range: @-178000000@ years to @178000000@ years.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-INTERVAL-INPUT).
data Interval = Interval
  { months :: Int32,
    days :: Int32,
    micros :: Int64
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Interval)

instance Bounded Interval where
  minBound =
    Interval
      { months = -178000000 * 12,
        days = 0,
        micros = 0
      }
  maxBound =
    Interval
      { months = 178000000 * 12,
        days = 0,
        micros = 0
      }

instance Arbitrary Interval where
  arbitrary = do
    micros <- QuickCheck.choose (-999_999, 999_999)
    days <- QuickCheck.choose (-daysPerMonth, daysPerMonth)
    months <- QuickCheck.choose ((minBound @Interval).months, (maxBound @Interval).months)
    pure (max minBound (min maxBound (Interval {..})))

instance IsScalar Interval where
  typeName = Tagged "interval"
  baseOid = Tagged (Just 1186)
  arrayOid = Tagged (Just 1187)
  typeParams = Tagged []
  binaryEncoder (Interval {..}) =
    mconcat [Write.bInt64 micros, Write.bInt32 days, Write.bInt32 months]
  binaryDecoder = PtrPeeker.fixed do
    micros <- PtrPeeker.beSignedInt8
    days <- PtrPeeker.beSignedInt4
    months <- PtrPeeker.beSignedInt4
    pure (Right (Interval {..}))

  -- Renders in "format with designators" of ISO-8601 as per [the Postgres documentation](https://www.postgresql.org/docs/current/datatype-datetime.html#DATATYPE-INTERVAL-INPUT).
  --
  -- >P quantity unit [ quantity unit ...] [ T [ quantity unit ...]]
  textualEncoder (Interval {..}) =
    let monthsSign = if months < 0 then "-" else ""
        daysSign = if days < 0 then "-" else ""
        microsSign = if micros < 0 then "-" else ""
        absMonths = abs months
        absDays = abs days
        yearsPart =
          if absMonths >= 12
            then monthsSign <> TextBuilder.decimal (absMonths `div` 12) <> "Y"
            else mempty
        monthsPart =
          if absMonths `mod` 12 /= 0
            then monthsSign <> TextBuilder.decimal (absMonths `mod` 12) <> "M"
            else mempty
        daysPart =
          if absDays /= 0
            then daysSign <> TextBuilder.decimal absDays <> "D"
            else mempty

        totalMicros = abs micros
        hours = totalMicros `div` (60 * 60 * 1000000)
        remainingMicros = totalMicros `mod` (60 * 60 * 1000000)
        minutes = remainingMicros `div` (60 * 1000000)
        seconds = remainingMicros `mod` (60 * 1000000)

        hoursPart =
          if hours /= 0
            then microsSign <> TextBuilder.decimal hours <> "H"
            else mempty
        minutesPart =
          if minutes /= 0
            then microsSign <> TextBuilder.decimal minutes <> "M"
            else mempty
        secondsPart =
          if seconds /= 0
            then
              microsSign
                <> if seconds `mod` 1000000 == 0
                  then TextBuilder.decimal (seconds `div` 1000000) <> "S"
                  else
                    TextBuilder.decimal (seconds `div` 1000000)
                      <> "."
                      <> TextBuilder.fixedLengthDecimal 6 (seconds `mod` 1000000)
                      <> "S"
            else mempty

        timePart = hoursPart <> minutesPart <> secondsPart
        tPrefix = if timePart /= mempty then "T" else mempty
        datePart = yearsPart <> monthsPart <> daysPart
     in if datePart == mempty && timePart == mempty
          then "PT0S"
          else "P" <> datePart <> tPrefix <> timePart

  -- Parse ISO-8601 duration format: P[n]Y[n]M[n]DT[n]H[n]M[n]S
  -- Also parse PostgreSQL's native interval format: "N years M mons D days HH:MM:SS.micros"
  textualDecoder = parseISO8601Format <|> parsePostgresFormat
    where
      -- Shared parser for signed numbers
      parseSignedNumber = do
        sign <- Attoparsec.option 1 (((-1) <$ Attoparsec.char '-') <|> (1 <$ Attoparsec.char '+'))
        n <- Attoparsec.decimal :: Attoparsec.Parser Integer
        pure (sign, n)

      -- Parse PostgreSQL's native format like "130331443 years 4 mons 4 days 00:00:00.334796"
      -- or simpler forms like "00:00:00" or "1 day"
      parsePostgresFormat = do
        -- Try to parse date components first
        (years, months, days) <- parsePostgresDatePart (0 :: Integer) (0 :: Integer) (0 :: Integer)
        -- Parse time part HH:MM:SS.micros (optional)
        micros <- Attoparsec.option 0 parsePostgresTime
        let totalMonths = fromIntegral (years * 12 + months)
            totalDays = fromIntegral days
        pure (Interval totalMonths totalDays micros)

      parsePostgresDatePart years months days = do
        -- Peek to see if we have a number or time separator coming
        mc <- Attoparsec.peekChar
        case mc of
          Nothing -> pure (years, months, days)
          Just '-' -> do
            -- Check if this is a negative date component or the start of negative time
            -- Try to parse as a date component, if it fails, treat as time
            result <- optional $ Attoparsec.try parseUnitValue'
            case result of
              Just (y', m', d') -> parsePostgresDatePart (years + y') (months + m') (days + d')
              Nothing -> pure (years, months, days) -- Not a date component, must be time
          Just '+' -> do
            -- Explicit positive sign - parse as date component
            result <- optional $ Attoparsec.try parseUnitValue'
            case result of
              Just (y', m', d') -> parsePostgresDatePart (years + y') (months + m') (days + d')
              Nothing -> pure (years, months, days)
          Just c | isDigit c -> do
            -- Use try so that if this isn't a date component, we backtrack
            result <- optional $ Attoparsec.try $ do
              n <- Attoparsec.decimal :: Attoparsec.Parser Integer
              mc2 <- Attoparsec.peekChar
              case mc2 of
                Just ' ' -> do
                  Attoparsec.skipSpace
                  unit <- Attoparsec.takeWhile1 isAlpha
                  Attoparsec.skipSpace
                  pure (n, unit)
                _ -> fail "Not a date component"
            case result of
              Just (n, unit) ->
                case unit of
                  "year" -> parsePostgresDatePart (years + n) months days
                  "years" -> parsePostgresDatePart (years + n) months days
                  "mon" -> parsePostgresDatePart years (months + n) days
                  "mons" -> parsePostgresDatePart years (months + n) days
                  "day" -> parsePostgresDatePart years months (days + n)
                  "days" -> parsePostgresDatePart years months (days + n)
                  _ -> fail ("Unknown interval unit: " ++ Text.unpack unit)
              Nothing -> pure (years, months, days) -- Not a date component, probably time
          Just _ -> pure (years, months, days) -- Something else, stop
      parseUnitValue' = do
        (sign, n) <- parseSignedNumber
        Attoparsec.skipSpace
        unit <- Attoparsec.takeWhile1 isAlpha
        Attoparsec.skipSpace
        case unit of
          "year" -> pure (sign * n, 0, 0)
          "years" -> pure (sign * n, 0, 0)
          "mon" -> pure (0, sign * n, 0)
          "mons" -> pure (0, sign * n, 0)
          "day" -> pure (0, 0, sign * n)
          "days" -> pure (0, 0, sign * n)
          _ -> fail ("Unknown interval unit: " ++ Text.unpack unit)

      parsePostgresTime = do
        sign <- Attoparsec.option 1 (((-1) <$ Attoparsec.char '-') <|> (1 <$ Attoparsec.char '+'))
        hours <- Attoparsec.decimal
        _ <- Attoparsec.char ':'
        mins <- Attoparsec.decimal
        _ <- Attoparsec.char ':'
        secs <- Attoparsec.decimal
        micros <-
          Attoparsec.option
            0
            ( do
                _ <- Attoparsec.char '.'
                digits <- Attoparsec.takeWhile1 isDigit
                let paddedDigits = take 6 (Text.unpack digits ++ repeat '0')
                    microsVal = foldl' (\acc d -> acc * 10 + fromIntegral (digitToInt d)) 0 paddedDigits
                pure microsVal
            )
        let totalMicros = hours * 3600_000_000 + mins * 60_000_000 + secs * 1_000_000 + micros
        pure (sign * totalMicros)

      parseISO8601Format = do
        _ <- Attoparsec.char 'P'
        -- Parse date part
        (years, monthsPart, daysPart) <- parseDatePart (0 :: Integer) (0 :: Integer) (0 :: Integer)
        -- Parse time part (optional)
        (hours, mins, secs, microsPart) <-
          Attoparsec.option (0, 0, 0, 0) (Attoparsec.char 'T' *> parseTimePart 0 0 0 0)
        let totalMonths = fromIntegral (years * 12 + monthsPart)
            totalDays = fromIntegral daysPart
            totalMicros = fromIntegral $ hours * 3600_000_000 + mins * 60_000_000 + secs * 1_000_000 + microsPart
        pure (Interval totalMonths totalDays totalMicros)

      parseDatePart years months days = do
        mc <- Attoparsec.peekChar
        case mc of
          Just 'T' -> pure (years, months, days)
          Nothing -> pure (years, months, days)
          Just c | isDigit c || c == '-' -> do
            (sign, n) <- parseSignedNumber
            designator <- Attoparsec.satisfy (`elem` ['Y', 'M', 'D'])
            case designator of
              'Y' -> parseDatePart (years + sign * n) months days
              'M' -> parseDatePart years (months + sign * n) days
              'D' -> parseDatePart years months (days + sign * n)
              _ -> fail "Unexpected designator"
          _ -> pure (years, months, days)
      parseTimePart hours mins secs micros = do
        mc <- Attoparsec.peekChar
        case mc of
          Nothing -> pure (hours, mins, secs, micros)
          Just c | isDigit c || c == '-' -> do
            (sign, n) <- parseSignedNumber
            -- Check for fractional seconds
            hasFraction <- Attoparsec.option False (True <$ Attoparsec.char '.')
            if hasFraction
              then do
                fracDigits <- Attoparsec.takeWhile1 isDigit
                _ <- Attoparsec.char 'S'
                let paddedDigits = take 6 (Text.unpack fracDigits ++ repeat '0')
                    microsFrac = foldl' (\acc d -> acc * 10 + fromIntegral (digitToInt d)) 0 paddedDigits :: Integer
                    totalMicrosForSecs = (sign * n) * 1_000_000 + sign * microsFrac
                parseTimePart hours mins secs (micros + totalMicrosForSecs)
              else do
                designator <- Attoparsec.satisfy (`elem` ['H', 'M', 'S'])
                case designator of
                  'H' -> parseTimePart (hours + (sign * n)) mins secs micros
                  'M' -> parseTimePart hours (mins + (sign * n)) secs micros
                  'S' -> parseTimePart hours mins (secs + (sign * n)) micros
                  _ -> fail "Unexpected time designator"
          _ -> pure (hours, mins, secs, micros)

-- | Safe conversion from tuple representation (months, days, microseconds) to Interval.
-- Validates that the input values are within PostgreSQL's valid range for intervals.
instance IsSome (Int32, Int32, Int64) Interval where
  to (Interval {..}) = (months, days, micros)
  maybeFrom (months, days, micros) =
    let interval = Interval {..}
     in if interval >= minBound && interval <= maxBound
          then Just interval
          else Nothing

-- | Total conversion from tuple representation (months, days, microseconds) to Interval.
-- Preserves the structured representation while clamping to valid ranges.
instance IsMany (Int32, Int32, Int64) Interval where
  onfrom (months, days, micros) =
    let interval = Interval {..}
     in -- First try the direct interval, then clamp to bounds if needed
        if interval >= minBound && interval <= maxBound
          then interval
          else max minBound (min maxBound interval)

fromMicros :: Integer -> Interval
fromMicros =
  evalState do
    micros <- fromIntegral <$> state (swap . flip divMod microsPerDay)
    days <- fromIntegral <$> state (swap . flip divMod daysPerMonth)
    months <- fromIntegral <$> get
    pure Interval {..}

toMicros :: Interval -> Integer
toMicros Interval {..} =
  fromIntegral micros + microsPerDay * (fromIntegral days + daysPerMonth * fromIntegral months)

toPicos :: Interval -> Integer
toPicos = (1_000_000 *) . toMicros

toDiffTime :: Interval -> DiffTime
toDiffTime = picosecondsToDiffTime . toPicos

microsPerDay :: (Num a) => a
microsPerDay = 1_000_000 * 60 * 60 * 24

daysPerMonth :: (Num a) => a
daysPerMonth = 30
