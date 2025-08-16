module PrimitiveLayer.Primitives.Timestamptz (Timestamptz) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

newtype Timestamptz = Timestamptz Int64
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Timestamptz)

instance Arbitrary Timestamptz where
  arbitrary = Timestamptz <$> QuickCheck.choose (0, maxBound)

instance Primitive Timestamptz where
  typeName = Tagged "timestamptz"
  baseOid = Tagged 1184
  arrayOid = Tagged 1185
  binaryEncoder (Timestamptz micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    pure (Right (Timestamptz microseconds))
  textualEncoder (toUtcTime -> utcTime) =
    TextBuilder.string (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z" utcTime)

-- PostgreSQL timestamptz epoch is 2000-01-01 00:00:00 UTC
postgresUtcEpoch :: Time.UTCTime
postgresUtcEpoch = Time.UTCTime (Time.fromGregorian 2000 1 1) 0

toUtcTime :: Timestamptz -> Time.UTCTime
toUtcTime (Timestamptz micros) =
  let diffTime = fromIntegral micros / 1_000_000
   in Time.addUTCTime diffTime postgresUtcEpoch
