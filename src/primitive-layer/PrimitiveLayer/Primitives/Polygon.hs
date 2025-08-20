-- | PostgreSQL @polygon@ type.
-- Represents a closed geometric polygon in 2D space.
module PrimitiveLayer.Primitives.Polygon (Polygon (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @polygon@ type representing a closed polygon in 2D space.
-- A polygon is defined by a series of vertices (points).
-- The polygon is automatically closed (the last point connects to the first).
-- Stored as the number of points followed by the point coordinates.
newtype Polygon = Polygon
  { polygonPoints :: [(Double, Double)]
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Polygon)

instance Arbitrary Polygon where
  arbitrary = do
    numPoints <- QuickCheck.choose (3, 10) -- Polygons need at least 3 points
    points <- QuickCheck.vectorOf numPoints arbitrary
    pure (Polygon points)
  shrink (Polygon points) = [Polygon points' | points' <- shrink points, length points' >= 3]

instance Primitive Polygon where
  typeName = Tagged "polygon"
  baseOid = Tagged 604
  arrayOid = Tagged 1027
  binaryEncoder (Polygon points) =
    let numPoints = fromIntegral (length points) :: Int32
        pointsEncoded = foldMap encodePoint points
     in mconcat
          [ Write.bInt32 numPoints,
            pointsEncoded
          ]
    where
      encodePoint (x, y) =
        mconcat
          [ Write.bWord64 (castDoubleToWord64 x),
            Write.bWord64 (castDoubleToWord64 y)
          ]
  binaryDecoder = do
    numPoints <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
    points <- PeekyBlinders.statically (replicateM (fromIntegral numPoints) decodePoint)
    pure (Right (Polygon points))
    where
      decodePoint = do
        x <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
        y <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
        pure (x, y)
  textualEncoder (Polygon points) =
    "(" <> TextBuilder.intercalate "," (map encodePoint points) <> ")"
    where
      encodePoint (x, y) =
        "(" <> TextBuilder.string (show x) <> "," <> TextBuilder.string (show y) <> ")"

-- | Convert from a list of points to a Polygon.
-- This is always safe since both represent the same data.
instance IsSome [(Double, Double)] Polygon where
  to (Polygon points) = points
  maybeFrom points = Just (Polygon points)

-- | Convert from a Polygon to a list of points.
-- This is always safe since both represent the same data.
instance IsSome Polygon [(Double, Double)] where
  to points = Polygon points
  maybeFrom (Polygon points) = Just points

-- | Direct conversion from list of points to Polygon.
-- This is a total conversion as it always succeeds.
instance IsMany [(Double, Double)] Polygon where
  from points = Polygon points

-- | Direct conversion from Polygon to list of points.
-- This is a total conversion as it always succeeds.
instance IsMany Polygon [(Double, Double)] where
  from (Polygon points) = points

-- | Bidirectional conversion between list of points and Polygon.
instance Is [(Double, Double)] Polygon

instance Is Polygon [(Double, Double)]
