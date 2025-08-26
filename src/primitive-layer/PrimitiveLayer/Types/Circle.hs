module PrimitiveLayer.Types.Circle (Circle) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @circle@ type. Circle in 2D plane.
--
-- Represents a circle with center coordinates and radius.
-- Gets stored as three @64@-bit floating point numbers (@x@,@y@,@radius@) in PostgreSQL.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-geometric.html#DATATYPE-CIRCLE).
data Circle = Circle
  { -- | Center x coordinate
    circleCenterX :: Double,
    -- | Center y coordinate
    circleCenterY :: Double,
    -- | Circle radius (must be non-negative)
    circleRadius :: Double
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Circle)

instance Arbitrary Circle where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    r <- QuickCheck.suchThat arbitrary (>= 0)
    pure (Circle x y r)
  shrink (Circle x y r) = do
    x' <- shrink x
    y' <- shrink y
    r' <- abs <$> shrink r
    pure (Circle x' y' r')

instance Mapping Circle where
  typeName = Tagged "circle"
  baseOid = Tagged 718
  arrayOid = Tagged 719
  binaryEncoder (Circle x y r) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x),
        Write.bWord64 (castDoubleToWord64 y),
        Write.bWord64 (castDoubleToWord64 r)
      ]
  binaryDecoder = PtrPeeker.fixed do
    x <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
    y <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
    r <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
    pure (Right (Circle x y r))
  textualEncoder (Circle x y r) =
    mconcat
      [ "<(",
        TextBuilder.string (show x),
        ",",
        TextBuilder.string (show y),
        "),",
        TextBuilder.string (show r),
        ">"
      ]

-- | Conversion from (x, y, radius) to Circle.
-- The radius is validated to be non-negative.
instance IsSome (Double, Double, Double) Circle where
  to (Circle x y r) = (x, y, r)
  maybeFrom (x, y, r) =
    if r < 0
      then Nothing
      else Just (Circle x y r)

-- | Conversion from (x, y, radius) to Circle.
-- The radius is made non-negative by taking the absolute value.
instance IsMany (Double, Double, Double) Circle where
  onfrom (x, y, r) = Circle x y (abs r)
