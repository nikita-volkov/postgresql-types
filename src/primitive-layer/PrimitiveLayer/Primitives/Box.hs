-- | PostgreSQL @box@ type.
-- Represents a rectangular box as two corner points in PostgreSQL.
module PrimitiveLayer.Primitives.Box (Box (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @box@ type representing a rectangular box defined by two opposite corners.
-- Stored as four 64-bit floating point numbers (x1,y1),(x2,y2) in PostgreSQL.
-- The box is normalized so that x1 <= x2 and y1 <= y2.
data Box = Box
  { -- | Lower-left x coordinate
    boxX1 :: !Double,
    -- | Lower-left y coordinate
    boxY1 :: !Double,
    -- | Upper-right x coordinate
    boxX2 :: !Double,
    -- | Upper-right y coordinate
    boxY2 :: !Double
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Box)

instance Arbitrary Box where
  arbitrary = do
    x1 <- arbitrary
    y1 <- arbitrary
    x2 <- arbitrary
    y2 <- arbitrary
    -- Normalize the box so x1 <= x2 and y1 <= y2
    pure $ Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
  shrink (Box x1 y1 x2 y2) =
    [ Box (min x1' x2') (min y1' y2') (max x1' x2') (max y1' y2')
    | (x1', y1', x2', y2') <- shrink (x1, y1, x2, y2)
    ]

instance Primitive Box where
  typeName = Tagged "box"
  baseOid = Tagged 603
  arrayOid = Tagged 1020
  binaryEncoder (Box x1 y1 x2 y2) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x2),
        Write.bWord64 (castDoubleToWord64 y2),
        Write.bWord64 (castDoubleToWord64 x1),
        Write.bWord64 (castDoubleToWord64 y1)
      ]
  binaryDecoder = do
    x2 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    y2 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    x1 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    y1 <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    pure (Right (Box x1 y1 x2 y2))
  textualEncoder (Box x1 y1 x2 y2) =
    "("
      <> TextBuilder.string (show x1)
      <> ","
      <> TextBuilder.string (show y1)
      <> "),"
      <> "("
      <> TextBuilder.string (show x2)
      <> ","
      <> TextBuilder.string (show y2)
      <> ")"

-- | Convert from two Points (lower-left and upper-right) to a Box.
-- Input is normalized to ensure x1 <= x2 and y1 <= y2.
instance IsSome ((Double, Double), (Double, Double)) Box where
  to (Box x1 y1 x2 y2) = ((x1, y1), (x2, y2))
  maybeFrom ((x1, y1), (x2, y2)) = Just (Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))

-- | Convert from a Box to two Points (lower-left and upper-right).
-- Always returns the normalized form.
instance IsSome Box ((Double, Double), (Double, Double)) where
  to ((x1, y1), (x2, y2)) = Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
  maybeFrom (Box x1 y1 x2 y2) = Just ((x1, y1), (x2, y2))

-- | Direct conversion from two points to Box.
-- Input is normalized to ensure valid box representation.
instance IsMany ((Double, Double), (Double, Double)) Box where
  from ((x1, y1), (x2, y2)) = Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)

-- | Direct conversion from Box to two points.
-- Always returns the normalized form.
instance IsMany Box ((Double, Double), (Double, Double)) where
  from (Box x1 y1 x2 y2) = ((x1, y1), (x2, y2))

-- | Bidirectional conversion between two points and Box.
instance Is ((Double, Double), (Double, Double)) Box

instance Is Box ((Double, Double), (Double, Double))
