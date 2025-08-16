module PrimitiveLayer.Primitives.Timestamp (Timestamp) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

newtype Timestamp = Timestamp Int64
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Timestamp)

instance Arbitrary Timestamp where
  arbitrary = Timestamp <$> QuickCheck.choose (0, maxBound)

instance Primitive Timestamp where
  typeName = Tagged "timestamp"
  baseOid = Tagged 1114
  arrayOid = Tagged 1115
  binaryEncoder (Timestamp micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    pure (Right (Timestamp microseconds))
  textualEncoder (toLocalTime -> localTime) =
    TextBuilder.string (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" localTime)

-- PostgreSQL timestamp epoch is 2000-01-01 00:00:00
postgresTimestampEpoch :: Time.LocalTime
postgresTimestampEpoch = Time.LocalTime (Time.fromGregorian 2000 1 1) Time.midnight

toLocalTime :: Timestamp -> Time.LocalTime
toLocalTime (Timestamp micros) =
  let diffTime = fromIntegral micros / 1_000_000
   in Time.addLocalTime diffTime postgresTimestampEpoch
