{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Polygon (Polygon) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Vector.Unboxed as UnboxedVector
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @polygon@ type. Closed geometric path in 2D plane.
--
-- Represented as a series of vertices (points).
-- The polygon is automatically closed (the last point connects to the first).
-- Stored as the number of points followed by the point coordinates.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-geometric.html#DATATYPE-POLYGON).
newtype Polygon = Polygon
  { polygonPoints :: UnboxedVector.Vector (Double, Double)
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Polygon)

instance Arbitrary Polygon where
  arbitrary = do
    size <- QuickCheck.getSize
    -- Polygons need at least 3 points
    numPoints <- QuickCheck.choose (3, max 3 size)
    points <- UnboxedVector.fromList <$> QuickCheck.vectorOf numPoints arbitrary
    pure (Polygon points)

  shrink (Polygon points) = [Polygon points' | points' <- shrink points, UnboxedVector.length points' >= 3]

instance IsScalar Polygon where
  typeName = Tagged "polygon"
  baseOid = Tagged (Just 604)
  arrayOid = Tagged (Just 1027)
  typeParams = Tagged []
  binaryEncoder (Polygon points) =
    let numPoints = fromIntegral (UnboxedVector.length points) :: Int32
        pointsEncoded = UnboxedVector.foldMap encodePoint points
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
    numPoints <- PtrPeeker.fixed PtrPeeker.beSignedInt4
    points <- UnboxedVector.replicateM (fromIntegral numPoints) decodePoint
    pure (Right (Polygon points))
    where
      decodePoint = PtrPeeker.fixed do
        x <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
        y <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
        pure (x, y)
  textualEncoder (Polygon points) =
    "(" <> TextBuilder.intercalateMap "," encodePoint (UnboxedVector.toList points) <> ")"
    where
      encodePoint (x, y) =
        "(" <> TextBuilder.string (printf "%g" x) <> "," <> TextBuilder.string (printf "%g" y) <> ")"
  textualDecoder = do
    _ <- Attoparsec.char '('
    points <- parsePoint `Attoparsec.sepBy1` Attoparsec.char ','
    _ <- Attoparsec.char ')'
    pure (Polygon (UnboxedVector.fromList points))
    where
      parsePoint = do
        _ <- Attoparsec.char '('
        x <- Attoparsec.double
        _ <- Attoparsec.char ','
        y <- Attoparsec.double
        _ <- Attoparsec.char ')'
        pure (x, y)

-- | Conversion to a list of points. At least 3 points are required to form a valid polygon.
instance IsSome (UnboxedVector.Vector (Double, Double)) Polygon where
  to (Polygon points) = points
  maybeFrom vector =
    if UnboxedVector.length vector >= 3
      then Just (Polygon vector)
      else Nothing

-- | Conversion to a list of points. At least 3 points are required to form a valid polygon.
instance IsSome [(Double, Double)] Polygon where
  to (Polygon points) = UnboxedVector.toList points
  maybeFrom = maybeFrom . UnboxedVector.fromList
