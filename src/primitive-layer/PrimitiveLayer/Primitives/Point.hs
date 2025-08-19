-- | PostgreSQL @point@ type.
-- Represents a 2D geometric point as (x,y) coordinates in PostgreSQL.
module PrimitiveLayer.Primitives.Point (Point (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @point@ type representing a 2D point with (x,y) coordinates.
-- Stored as two 64-bit floating point numbers (float8) in PostgreSQL.
data Point = Point
  { pointX :: Double,
    pointY :: Double
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Point)

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary
  shrink (Point x y) = [Point x' y' | (x', y') <- shrink (x, y)]

instance Primitive Point where
  typeName = Tagged "point"
  baseOid = Tagged 600
  arrayOid = Tagged 1017
  binaryEncoder (Point x y) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x),
        Write.bWord64 (castDoubleToWord64 y)
      ]
  binaryDecoder = do
    x <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    y <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    pure (Right (Point x y))
  textualEncoder (Point x y) =
    "(" <> TextBuilder.string (show x) <> "," <> TextBuilder.string (show y) <> ")"

-- | Convert from a tuple of doubles to a Point.
-- This is always safe since both represent the same data.
instance IsSome (Double, Double) Point where
  to (Point x y) = (x, y)
  maybeFrom (x, y) = Just (Point x y)

-- | Convert from a Point to a tuple of doubles.
-- This is always safe since both represent the same data.
instance IsSome Point (Double, Double) where
  to (x, y) = Point x y
  maybeFrom (Point x y) = Just (x, y)

-- | Direct conversion from tuple to Point.
-- This is a total conversion as it always succeeds.
instance IsMany (Double, Double) Point where
  from (x, y) = Point x y

-- | Direct conversion from Point to tuple.
-- This is a total conversion as it always succeeds.
instance IsMany Point (Double, Double) where
  from (Point x y) = (x, y)

-- | Bidirectional conversion between tuple and Point.
instance Is (Double, Double) Point

instance Is Point (Double, Double)
