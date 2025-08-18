-- | PostgreSQL @date@ type.
-- Represents a calendar date (without time) in PostgreSQL.
module PrimitiveLayer.Primitives.Date (Date) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @date@ type wrapper around 'Data.Time.Day'.
newtype Date = Date Time.Day
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Date)

-- PostgreSQL epoch is 2000-01-01, but Haskell's Day is based on Gregorian calendar with epoch 0000-01-01
postgresEpoch :: Time.Day
postgresEpoch = Time.fromGregorian 2000 1 1

instance Primitive Date where
  typeName = Tagged "date"
  baseOid = Tagged 1082
  arrayOid = Tagged 1182
  binaryEncoder (Date day) =
    let daysSincePostgresEpoch = fromIntegral (Time.diffDays day postgresEpoch)
     in Write.bInt32 daysSincePostgresEpoch
  binaryDecoder = do
    daysSinceEpoch <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
    let day = Time.addDays (fromIntegral daysSinceEpoch) postgresEpoch
    pure (Right (Date day))
  textualEncoder (Date day) = TextBuilder.text (fromString (Time.showGregorian day))

-- | Direct conversion from 'Data.Time.Day'.
-- This is always safe since both types represent the same date concept.
instance IsSome Time.Day Date where
  to (Date day) = day
  maybeFrom = Just . Date

-- | Direct conversion from 'Data.Time.Day'.
-- This is a total conversion as it always succeeds.
instance IsMany Time.Day Date where
  from = Date
