module PrimitiveLayer.Primitives.Path (Path (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | @path@. Geometric path in 2D plane (open or closed).
--
-- PostgreSQL @path@ type representing a geometric path in 2D space.
-- A path is a series of connected points, which can be either open or closed.
-- The first byte indicates if the path is closed (@1@) or open (@0@).
-- This is followed by the number of points and then the point coordinates.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-geometric.html#DATATYPE-PATH)
data Path = Path
  { pathClosed :: Bool,
    pathPoints :: [(Double, Double)]
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Path)

instance Arbitrary Path where
  arbitrary = do
    closed <- arbitrary
    numPoints <- QuickCheck.choose (2, 10) -- Paths need at least 2 points
    points <- QuickCheck.vectorOf numPoints arbitrary
    pure (Path closed points)
  shrink (Path closed points) =
    [Path closed' points' | (closed', points') <- shrink (closed, points), length points' >= 2]

instance Primitive Path where
  typeName = Tagged "path"
  baseOid = Tagged 602
  arrayOid = Tagged 1019
  binaryEncoder (Path closed points) =
    let closedByte = if closed then 1 else 0 :: Word8
        numPoints = fromIntegral (length points) :: Int32
        pointsEncoded = foldMap encodePoint points
     in mconcat
          [ Write.word8 closedByte,
            Write.bInt32 numPoints,
            pointsEncoded
          ]
    where
      encodePoint (x, y) =
        mconcat
          [ Write.bWord64 (castDoubleToWord64 x),
            Write.bWord64 (castDoubleToWord64 y)
          ]
  binaryDecoder = do
    closedByte <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    numPoints <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
    points <- PeekyBlinders.statically (replicateM (fromIntegral numPoints) decodePoint)
    let closed = closedByte /= 0
    pure (Right (Path closed points))
    where
      decodePoint = do
        x <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
        y <- castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8
        pure (x, y)
  textualEncoder (Path closed points) =
    let openChar = if closed then "(" else "["
        closeChar = if closed then ")" else "]"
        pointsStr = TextBuilder.intercalate "," (map encodePoint points)
     in openChar <> pointsStr <> closeChar
    where
      encodePoint (x, y) =
        "(" <> TextBuilder.string (show x) <> "," <> TextBuilder.string (show y) <> ")"

-- | Convert from a tuple of Bool and list of points to a Path.
-- This is always safe since both represent the same data.
instance IsSome (Bool, [(Double, Double)]) Path where
  to (Path closed points) = (closed, points)
  maybeFrom (closed, points) = Just (Path closed points)

-- | Convert from a Path to a tuple of Bool and list of points.
-- This is always safe since both represent the same data.
instance IsSome Path (Bool, [(Double, Double)]) where
  to (closed, points) = Path closed points
  maybeFrom (Path closed points) = Just (closed, points)

-- | Direct conversion from tuple to Path.
-- This is a total conversion as it always succeeds.
instance IsMany (Bool, [(Double, Double)]) Path where
  from (closed, points) = Path closed points

-- | Direct conversion from Path to tuple.
-- This is a total conversion as it always succeeds.
instance IsMany Path (Bool, [(Double, Double)]) where
  from (Path closed points) = (closed, points)

-- | Bidirectional conversion between tuple and Path.
instance Is (Bool, [(Double, Double)]) Path

instance Is Path (Bool, [(Double, Double)])
