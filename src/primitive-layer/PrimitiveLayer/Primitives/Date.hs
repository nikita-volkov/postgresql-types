module PrimitiveLayer.Primitives.Date (Date (..)) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

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
