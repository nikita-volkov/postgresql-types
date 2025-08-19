-- | PostgreSQL @circle@ type.
-- Represents a circle with center point and radius in PostgreSQL.
module PrimitiveLayer.Primitives.Circle (Circle (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @circle@ type representing a circle with center coordinates and radius.
-- Stored as three 64-bit floating point numbers (x,y,radius) in PostgreSQL.
data Circle = Circle
  { -- | Center x coordinate
    circleCenterX :: !Double,
    -- | Center y coordinate
    circleCenterY :: !Double,
    -- | Circle radius (must be non-negative)
    circleRadius :: !Double
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Circle)

instance Arbitrary Circle where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    r <- abs <$> arbitrary -- Ensure radius is non-negative
    pure (Circle x y r)
  shrink (Circle x y r) =
    [Circle x' y' (abs r') | (x', y', r') <- shrink (x, y, r)]

instance Primitive Circle where
  typeName = Tagged "circle"
  baseOid = Tagged 718
  arrayOid = Tagged 719
  binaryEncoder (Circle x y r) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x),
        Write.bWord64 (castDoubleToWord64 y),
        Write.bWord64 (castDoubleToWord64 r)
      ]
  binaryDecoder = PeekyBlinders.statically do
    x <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
    y <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
    r <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
    pure (Right (Circle x y r))
  textualEncoder (Circle x y r) =
    "<("
      <> TextBuilder.string (show x)
      <> ","
      <> TextBuilder.string (show y)
      <> "),"
      <> TextBuilder.string (show r)
      <> ">"

-- | Convert from a tuple of three doubles to a Circle.
-- The radius is made non-negative by taking the absolute value.
instance IsSome (Double, Double, Double) Circle where
  to (Circle x y r) = (x, y, r)
  maybeFrom (x, y, r) =
    if r < 0
      then Nothing
      else Just (Circle x y r)

-- | Direct conversion from tuple to Circle.
instance IsMany (Double, Double, Double) Circle where
  from (x, y, r) = Circle x y (abs r)
