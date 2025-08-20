module PrimitiveLayer.Primitives.Line (Line (..)) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | @line@. Infinite line in 2D plane.
--
-- PostgreSQL @line@ type representing an infinite line in 2D space.
-- The line is represented by the linear equation @Ax + By + C = 0@.
-- Stored as three @64@-bit floating point numbers (@A@, @B@, @C@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-geometric.html#DATATYPE-LINE)
data Line = Line
  { lineA :: Double,
    lineB :: Double,
    lineC :: Double
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Line)

instance Arbitrary Line where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    -- Ensure that at least one of A or B is non-zero for a valid line
    if a == 0 && b == 0
      then pure (Line 1 0 c) -- Default to vertical line x = -c
      else pure (Line a b c)
  shrink (Line a b c) =
    [ Line a' b' c'
    | (a', b', c') <- shrink (a, b, c),
      not (a' == 0 && b' == 0) -- Ensure shrunk values are also valid
    ]

instance Primitive Line where
  typeName = Tagged "line"
  baseOid = Tagged 628
  arrayOid = Tagged 629
  binaryEncoder (Line a b c) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 a),
        Write.bWord64 (castDoubleToWord64 b),
        Write.bWord64 (castDoubleToWord64 c)
      ]
  binaryDecoder = do
    a <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    b <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    c <- PeekyBlinders.statically (castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
    pure (Right (Line a b c))
  textualEncoder (Line a b c) =
    "{"
      <> TextBuilder.string (show a)
      <> ","
      <> TextBuilder.string (show b)
      <> ","
      <> TextBuilder.string (show c)
      <> "}"

-- | Convert from a tuple of three doubles to a Line.
-- This is always safe since both represent the same data.
instance IsSome (Double, Double, Double) Line where
  to (Line a b c) = (a, b, c)
  maybeFrom (a, b, c) = Just (Line a b c)

-- | Convert from a Line to a tuple of three doubles.
-- This is always safe since both represent the same data.
instance IsSome Line (Double, Double, Double) where
  to (a, b, c) = Line a b c
  maybeFrom (Line a b c) = Just (a, b, c)

-- | Direct conversion from tuple to Line.
-- This is a total conversion as it always succeeds.
instance IsMany (Double, Double, Double) Line where
  from (a, b, c) = Line a b c

-- | Direct conversion from Line to tuple.
-- This is a total conversion as it always succeeds.
instance IsMany Line (Double, Double, Double) where
  from (Line a b c) = (a, b, c)

-- | Bidirectional conversion between tuple and Line.
instance Is (Double, Double, Double) Line

instance Is Line (Double, Double, Double)
