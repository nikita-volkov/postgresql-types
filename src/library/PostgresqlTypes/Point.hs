module PostgresqlTypes.Point
  ( Point (..),

    -- * Accessors
    toX,
    toY,

    -- * Constructors
    fromCoordinates,
  )
where

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
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-geometric.html#DATATYPE-GEOMETRIC-POINTS).
data Point
  = Point
      -- | X coordinate
      Double
      -- | Y coordinate
      Double
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Point)

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary
  shrink (Point x y) = [Point x' y' | (x', y') <- shrink (x, y)]

instance IsScalar Point where
  schemaName = Tagged Nothing
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

-- * Accessors

-- | Extract the x coordinate.
toX :: Point -> Double
toX (Point x _) = x

-- | Extract the y coordinate.
toY :: Point -> Double
toY (Point _ y) = y

-- * Constructors

-- | Construct a PostgreSQL 'Point' from coordinates x and y.
fromCoordinates :: Double -> Double -> Point
fromCoordinates x y = Point x y
