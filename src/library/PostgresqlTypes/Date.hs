module PostgresqlTypes.Date (Date) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Time as Time
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @date@ type. Calendar date (year, month, day).
--
-- Range: @4713 BC@ to @5874897 AD@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-DATE).
newtype Date
  = -- | Days since PostgreSQL epoch (2000-01-01).
    Date Int32
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsScalar Date)

-- | PostgreSQL date range: 4713 BC to 5874897 AD.
--
-- Minimum date: 4713 BC January 1 = -4712-01-01 in ISO format.
--
-- Maximum date: 5874897 AD December 31 = 5874897-12-31 in ISO format.
instance Bounded Date where
  minBound = unsafeFromDay minDay
  maxBound = unsafeFromDay maxDay

-- | Custom Arbitrary instance that generates dates within PostgreSQL's supported range.
-- PostgreSQL supports dates from 4713 BC to 5874897 AD.
instance Arbitrary Date where
  arbitrary =
    QuickCheck.frequency
      [ ( 99,
          let minBase = coerce (minBound @Date)
              maxBase = coerce (maxBound @Date)
           in Date <$> QuickCheck.choose (minBase, maxBase)
        ),
        ( 1,
          do
            y <- QuickCheck.choose (-1, 1)
            m <- QuickCheck.choose (1, 12)
            d <- QuickCheck.choose (1, 31)
            pure (unsafeFromDay (Time.fromGregorian y m d))
        )
      ]

instance IsScalar Date where
  typeName = Tagged "date"
  baseOid = Tagged (Just 1082)
  arrayOid = Tagged (Just 1182)
  typeParams = Tagged []
  binaryEncoder (Date days) = Write.bInt32 days
  binaryDecoder = do
    days <- PtrPeeker.fixed PtrPeeker.beSignedInt4
    pure (Right (Date days))
  textualEncoder date =
    let day = toDay date
        (y, m, d) = Time.toGregorian day
        (y', bc) =
          if y <= 0
            then (negate y + 1, True)
            else (y, False)
     in mconcat
          [ if y' > 9999
              then TextBuilder.decimal y'
              else TextBuilder.fixedLengthDecimal 4 y',
            "-",
            TextBuilder.fixedLengthDecimal 2 m,
            "-",
            TextBuilder.fixedLengthDecimal 2 d,
            if bc then " BC" else ""
          ]
  textualDecoder = do
    -- Parse year (may have more than 4 digits for extreme dates)
    y <- Attoparsec.decimal
    _ <- Attoparsec.char '-'
    m <- twoDigits
    _ <- Attoparsec.char '-'
    d <- twoDigits
    -- Check for BC suffix
    bc <- Attoparsec.option False (True <$ (Attoparsec.skipSpace *> Attoparsec.string "BC"))
    let year = if bc then negate y + 1 else y
    -- Try to use fromGregorianValid for normal dates, fallback to custom calculation for extreme dates
    case Time.fromGregorianValid year m d of
      Just day -> pure (unsafeFromDay day)
      Nothing ->
        -- For extreme dates outside time library's range, compute days directly
        -- This is a simplified calculation that may not be perfectly accurate for all historical dates
        -- but matches PostgreSQL's handling of extreme dates
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
         in pure (Date (daysFromYears + fromIntegral monthDays + fromIntegral d - 1))
    where
      twoDigits = do
        a <- Attoparsec.digit
        b <- Attoparsec.digit
        pure (digitToInt a * 10 + digitToInt b)
      isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- | Mapping to @daterange@ type.
instance IsRangeElement Date where
  rangeTypeName = Tagged "daterange"
  rangeBaseOid = Tagged (Just 3912)
  rangeArrayOid = Tagged (Just 3913)

-- | Mapping to @datemultirange@ type.
instance IsMultirangeElement Date where
  multirangeTypeName = Tagged "datemultirange"
  multirangeBaseOid = Tagged (Just 4535)
  multirangeArrayOid = Tagged (Just 6155)

-- | Conversion to 'Data.Time.Day'.
instance IsSome Time.Day Date where
  to = toDay
  maybeFrom = refineFromDay

-- | Conversion from 'Data.Time.Day'.
instance IsMany Time.Day Date where
  onfrom = normalizeFromDay

-- | PostgreSQL epoch is 2000-01-01
postgresEpoch :: Time.Day
postgresEpoch = Time.fromGregorian 2000 1 1

-- | Convert Date to Day
toDay :: Date -> Time.Day
toDay (Date days) = Time.addDays (fromIntegral days) postgresEpoch

refineFromDay :: Time.Day -> Maybe Date
refineFromDay day
  | day < minDay || day > maxDay = Nothing
  | otherwise = Just (unsafeFromDay day)

normalizeFromDay :: Time.Day -> Date
normalizeFromDay day
  | day < minDay = minBound
  | day > maxDay = maxBound
  | otherwise = unsafeFromDay day

unsafeFromDay :: Time.Day -> Date
unsafeFromDay day = Date (fromIntegral (Time.diffDays day postgresEpoch))

minDay :: Time.Day
minDay = Time.fromGregorian (-4712) 1 1

maxDay :: Time.Day
maxDay = Time.fromGregorian 5874897 12 31
