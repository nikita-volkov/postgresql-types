{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Point (Point) where

import qualified Data.Attoparsec.Text as Attoparsec
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @point@ type. Geometric point in 2D plane.
--
-- Represented with (@x@,@y@) coordinates.
-- Stored as two @64@-bit floating point numbers (@float8@) in PostgreSQL.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-geometric.html#DATATYPE-GEOMETRIC-POINTS).
data Point = Point
  { pointX :: Double,
    pointY :: Double
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Point)

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary
  shrink (Point x y) = [Point x' y' | (x', y') <- shrink (x, y)]

instance IsScalar Point where
  typeName = Tagged "point"
  baseOid = Tagged (Just 600)
  arrayOid = Tagged (Just 1017)
  typeParams = Tagged []
  binaryEncoder (Point x y) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x),
        Write.bWord64 (castDoubleToWord64 y)
      ]
  binaryDecoder = do
    x <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    y <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    pure (Right (Point x y))
  textualEncoder (Point x y) =
    "(" <> TextBuilder.string (printf "%g" x) <> "," <> TextBuilder.string (printf "%g" y) <> ")"
  textualDecoder = do
    _ <- Attoparsec.char '('
    x <- Attoparsec.double
    _ <- Attoparsec.char ','
    y <- Attoparsec.double
    _ <- Attoparsec.char ')'
    pure (Point x y)

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
  onfrom (x, y) = Point x y

-- | Direct conversion from Point to tuple.
-- This is a total conversion as it always succeeds.
instance IsMany Point (Double, Double) where
  onfrom (Point x y) = (x, y)

-- | Bidirectional conversion between tuple and Point.
instance Is (Double, Double) Point

instance Is Point (Double, Double)
