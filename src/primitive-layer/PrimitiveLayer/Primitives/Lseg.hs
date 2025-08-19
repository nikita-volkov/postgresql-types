-- | PostgreSQL @lseg@ type.
-- Represents a line segment in 2D space defined by two endpoints.
module PrimitiveLayer.Primitives.Lseg (Lseg (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @lseg@ type representing a line segment in 2D space.
-- The line segment is defined by two endpoints, each with (x,y) coordinates.
-- Stored as four 64-bit floating point numbers: (x1, y1, x2, y2).
data Lseg = Lseg
  { lsegX1 :: Double,
    lsegY1 :: Double,
    lsegX2 :: Double,
    lsegY2 :: Double
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Lseg)

instance Arbitrary Lseg where
  arbitrary = Lseg <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Lseg x1 y1 x2 y2) =
    [Lseg x1' y1' x2' y2' | (x1', y1', x2', y2') <- shrink (x1, y1, x2, y2)]

instance Primitive Lseg where
  typeName = Tagged "lseg"
  baseOid = Tagged 601
  arrayOid = Tagged 1018
  binaryEncoder (Lseg x1 y1 x2 y2) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x1),
        Write.bWord64 (castDoubleToWord64 y1),
        Write.bWord64 (castDoubleToWord64 x2),
        Write.bWord64 (castDoubleToWord64 y2)
      ]
  binaryDecoder = do
    x1 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    y1 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    x2 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    y2 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    pure (Right (Lseg x1 y1 x2 y2))
  textualEncoder (Lseg x1 y1 x2 y2) =
    "[("
      <> TextBuilder.string (show x1)
      <> ","
      <> TextBuilder.string (show y1)
      <> "),"
      <> "("
      <> TextBuilder.string (show x2)
      <> ","
      <> TextBuilder.string (show y2)
      <> ")]"

-- | Convert from a tuple of two points to an Lseg.
-- This is always safe since both represent the same data.
instance IsSome ((Double, Double), (Double, Double)) Lseg where
  to (Lseg x1 y1 x2 y2) = ((x1, y1), (x2, y2))
  maybeFrom ((x1, y1), (x2, y2)) = Just (Lseg x1 y1 x2 y2)

-- | Convert from an Lseg to a tuple of two points.
-- This is always safe since both represent the same data.
instance IsSome Lseg ((Double, Double), (Double, Double)) where
  to ((x1, y1), (x2, y2)) = Lseg x1 y1 x2 y2
  maybeFrom (Lseg x1 y1 x2 y2) = Just ((x1, y1), (x2, y2))

-- | Direct conversion from tuple of points to Lseg.
-- This is a total conversion as it always succeeds.
instance IsMany ((Double, Double), (Double, Double)) Lseg where
  from ((x1, y1), (x2, y2)) = Lseg x1 y1 x2 y2

-- | Direct conversion from Lseg to tuple of points.
-- This is a total conversion as it always succeeds.
instance IsMany Lseg ((Double, Double), (Double, Double)) where
  from (Lseg x1 y1 x2 y2) = ((x1, y1), (x2, y2))

-- | Bidirectional conversion between tuple of points and Lseg.
instance Is ((Double, Double), (Double, Double)) Lseg

instance Is Lseg ((Double, Double), (Double, Double))
