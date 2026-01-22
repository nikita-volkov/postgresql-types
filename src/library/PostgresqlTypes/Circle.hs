module PostgresqlTypes.Circle
  ( Circle,

    -- * Accessors
    toCenterX,
    toCenterY,
    toRadius,

    -- * Constructors
    refineFromCenterAndRadius,
    normalizeFromCenterAndRadius,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @circle@ type. Circle in 2D plane.
--
-- Represents a circle with center coordinates and radius.
-- Gets stored as three @64@-bit floating point numbers (@x@,@y@,@radius@) in PostgreSQL.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-geometric.html#DATATYPE-CIRCLE).
data Circle
  = Circle
      -- | Center x coordinate
      Double
      -- | Center y coordinate
      Double
      -- | Circle radius (must be non-negative)
      Double
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Circle)

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

instance IsScalar Circle where
  schemaName = Tagged Nothing
  typeName = Tagged "circle"
  baseOid = Tagged (Just 718)
  arrayOid = Tagged (Just 719)
  typeParams = Tagged []
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
        TextBuilder.string (printf "%g" x),
        ",",
        TextBuilder.string (printf "%g" y),
        "),",
        TextBuilder.string (printf "%g" r),
        ">"
      ]
  textualDecoder = do
    _ <- Attoparsec.char '<'
    _ <- Attoparsec.char '('
    x <- Attoparsec.double
    _ <- Attoparsec.char ','
    y <- Attoparsec.double
    _ <- Attoparsec.char ')'
    _ <- Attoparsec.char ','
    r <- Attoparsec.double
    _ <- Attoparsec.char '>'
    pure (Circle x y r)

-- * Accessors

-- | Extract the center x coordinate.
toCenterX :: Circle -> Double
toCenterX (Circle x _ _) = x

-- | Extract the center y coordinate.
toCenterY :: Circle -> Double
toCenterY (Circle _ y _) = y

-- | Extract the radius.
toRadius :: Circle -> Double
toRadius (Circle _ _ r) = r

-- * Constructors

-- | Construct a PostgreSQL 'Circle' from (x, y, radius) with validation.
-- Returns 'Nothing' if radius is negative.
refineFromCenterAndRadius :: Double -> Double -> Double -> Maybe Circle
refineFromCenterAndRadius x y r =
  if r < 0
    then Nothing
    else Just (Circle x y r)

-- | Construct a PostgreSQL 'Circle' from (x, y, radius).
-- Makes radius non-negative by taking absolute value.
normalizeFromCenterAndRadius :: Double -> Double -> Double -> Circle
normalizeFromCenterAndRadius x y r = Circle x y (abs r)
