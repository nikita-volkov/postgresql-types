{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Timetz (Timetz) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import qualified PostgresqlTypes.Types.Timetz.Offset as Offset
import qualified PostgresqlTypes.Types.Timetz.Time as Time
import PostgresqlTypes.Via
import qualified PtrPeeker

-- | PostgreSQL @timetz@ type. Time of day with time zone.
--
-- Stored as microseconds since midnight and timezone offset in seconds.
--
-- Low value: @00:00:00+1559@. High value: @24:00:00-1559@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-TIMEZONES).
data Timetz = Timetz
  { -- | Time as microseconds since midnight (00:00:00)
    time :: Time.TimetzTime,
    -- | Timezone offset in seconds (positive is east of UTC, negative is west of UTC)
    offset :: Offset.TimetzOffset
  }
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
    -- Parse timezone offset: [+-]HH[:MM[:SS]]
    -- PostgreSQL omits minutes and seconds when they are zero
    sign <- (1 <$ Attoparsec.char '+') <|> ((-1) <$ Attoparsec.char '-')
    tzH <- twoDigits
    tzM <- Attoparsec.option 0 (Attoparsec.char ':' *> twoDigits)
    tzS <- Attoparsec.option 0 (Attoparsec.char ':' *> twoDigits)
    -- Build time and offset
    let timeMicros = fromIntegral h * 3600_000_000 + fromIntegral m * 60_000_000 + fromIntegral s * 1_000_000 + micros
        -- Note: PostgreSQL stores offset with inverted sign (positive means west of UTC)
        offsetSeconds = negate sign * (fromIntegral tzH * 3600 + fromIntegral tzM * 60 + fromIntegral tzS)
    case (Time.projectFromMicroseconds timeMicros, Offset.projectFromSeconds offsetSeconds) of
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

-- | Convert from a tuple of time in microseconds and timezone offset in seconds to Timetz.
instance IsSome (Int64, Int32) Timetz where
  to (Timetz time offset) =
    (Time.toMicroseconds time, Offset.toSeconds offset)
  maybeFrom (microseconds, offset) = do
    time <- Time.projectFromMicroseconds microseconds
    offset <- Offset.projectFromSeconds offset
    pure (Timetz time offset)

-- | Normalize from time in microseconds and timezone offset in seconds, ensuring valid time range.
instance IsMany (Int64, Int32) Timetz where
  onfrom (time, offset) =
    Timetz (Time.normalizeFromMicroseconds time) (Offset.normalizeFromSeconds offset)

instance IsSome (Time.TimetzTime, Offset.TimetzOffset) Timetz where
  to (Timetz time offset) = (time, offset)

instance IsSome Timetz (Time.TimetzTime, Offset.TimetzOffset) where
  to (time, offset) = Timetz time offset

instance IsMany (Time.TimetzTime, Offset.TimetzOffset) Timetz

instance IsMany Timetz (Time.TimetzTime, Offset.TimetzOffset)

instance Is (Time.TimetzTime, Offset.TimetzOffset) Timetz

instance Is Timetz (Time.TimetzTime, Offset.TimetzOffset)
