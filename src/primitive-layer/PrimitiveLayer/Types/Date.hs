module PrimitiveLayer.Types.Date (Date) where

import qualified Data.Time as Time
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @date@ type. Calendar date (year, month, day).
--
-- Range: @4713 BC@ to @5874897 AD@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-DATE).
newtype Date
  = -- | Days since PostgreSQL epoch (2000-01-01).
    Date Int32
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Date)

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

instance Mapping Date where
  typeName = Tagged "date"
  baseOid = Tagged 1082
  arrayOid = Tagged 1182
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

-- | Mapping to @daterange@ type.
instance RangeMapping Date where
  rangeTypeName = Tagged "daterange"
  rangeOid = Tagged 3912
  rangeArrayOid = Tagged 3913

-- | Mapping to @datemultirange@ type.
instance MultirangeMapping Date where
  multirangeTypeName = Tagged "datemultirange"
  multirangeOid = Tagged 4535
  multirangeArrayOid = Tagged 6155

-- | Conversion to 'Data.Time.Day'.
instance IsSome Time.Day Date where
  to = toDay
  maybeFrom = compileFromDay

-- | Conversion from 'Data.Time.Day'.
instance IsMany Time.Day Date where
  onfrom = normalizeFromDay

-- | PostgreSQL epoch is 2000-01-01
postgresEpoch :: Time.Day
postgresEpoch = Time.fromGregorian 2000 1 1

-- | Convert Date to Day
toDay :: Date -> Time.Day
toDay (Date days) = Time.addDays (fromIntegral days) postgresEpoch

compileFromDay :: Time.Day -> Maybe Date
compileFromDay day
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
