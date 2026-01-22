module PostgresqlTypes.Box
  ( Box,

    -- * Accessors
    toX1,
    toY1,
    toX2,
    toY2,

    -- * Constructors
    normalizeFromCorners,
    refineFromCorners,
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

-- | PostgreSQL @box@ type. Rectangular box in 2D plane.
--
-- Rectangular box defined by two opposite corners.
-- Stored as four @64@-bit floating point numbers (@x1@,@y1@),(@x2@,@y2@) in PostgreSQL.
-- The box is normalized so that @x1 <= x2@ and @y1 <= y2@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-geometric.html#DATATYPE-GEOMETRIC-BOXES).
data Box
  = Box
      -- | Lower-left x coordinate
      Double
      -- | Lower-left y coordinate
      Double
      -- | Upper-right x coordinate
      Double
      -- | Upper-right y coordinate
      Double
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Box)

instance Arbitrary Box where
  arbitrary = do
    x1 <- arbitrary
    y1 <- arbitrary
    x2 <- arbitrary
    y2 <- arbitrary
    pure (normalizeFromCorners x1 y1 x2 y2)

  shrink (Box x1 y1 x2 y2) =
    [ normalizeFromCorners x1' y1' x2' y2'
    | (x1', y1', x2', y2') <- shrink (x1, y1, x2, y2)
    ]

instance Hashable Box where
  hashWithSalt salt (Box x1 y1 x2 y2) =
    salt
      `hashWithSalt` castDoubleToWord64 x1
      `hashWithSalt` castDoubleToWord64 y1
      `hashWithSalt` castDoubleToWord64 x2
      `hashWithSalt` castDoubleToWord64 y2

instance IsScalar Box where
  schemaName = Tagged Nothing
  typeName = Tagged "box"
  baseOid = Tagged (Just 603)
  arrayOid = Tagged (Just 1020)
  typeParams = Tagged []
  binaryEncoder (Box x1 y1 x2 y2) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x2),
        Write.bWord64 (castDoubleToWord64 y2),
        Write.bWord64 (castDoubleToWord64 x1),
        Write.bWord64 (castDoubleToWord64 y1)
      ]
  binaryDecoder = do
    x2 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    y2 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    x1 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    y1 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    pure (Right (Box x1 y1 x2 y2))
  textualEncoder (Box x1 y1 x2 y2) =
    -- PostgreSQL returns coordinates as (upper-right),(lower-left)
    -- So we output (x2,y2),(x1,y1)
    mconcat
      [ "(",
        TextBuilder.string (printf "%g" x2),
        ",",
        TextBuilder.string (printf "%g" y2),
        "),(",
        TextBuilder.string (printf "%g" x1),
        ",",
        TextBuilder.string (printf "%g" y1),
        ")"
      ]
  textualDecoder = do
    _ <- Attoparsec.char '('
    x1 <- Attoparsec.double
    _ <- Attoparsec.char ','
    y1 <- Attoparsec.double
    _ <- Attoparsec.char ')'
    _ <- Attoparsec.char ','
    _ <- Attoparsec.char '('
    x2 <- Attoparsec.double
    _ <- Attoparsec.char ','
    y2 <- Attoparsec.double
    _ <- Attoparsec.char ')'
    -- PostgreSQL may return coordinates in any order, normalize to ensure x1 <= x2 and y1 <= y2
    pure (normalizeFromCorners x1 y1 x2 y2)

-- * Accessors

-- | Extract the lower-left x coordinate.
toX1 :: Box -> Double
toX1 (Box x1 _ _ _) = x1

-- | Extract the lower-left y coordinate.
toY1 :: Box -> Double
toY1 (Box _ y1 _ _) = y1

-- | Extract the upper-right x coordinate.
toX2 :: Box -> Double
toX2 (Box _ _ x2 _) = x2

-- | Extract the upper-right y coordinate.
toY2 :: Box -> Double
toY2 (Box _ _ _ y2) = y2

-- * Constructors

-- | Construct a PostgreSQL 'Box' from corners (x1, y1, x2, y2).
-- Normalizes coordinates to ensure lowerX <= upperX and lowerY <= upperY.
normalizeFromCorners :: Double -> Double -> Double -> Double -> Box
normalizeFromCorners x1 y1 x2 y2 =
  if x1 <= x2
    then
      if y1 <= y2
        then Box x1 y1 x2 y2
        else Box x1 y2 x2 y1
    else
      if y1 <= y2
        then Box x2 y1 x1 y2
        else Box x2 y2 x1 y1

-- | Construct a PostgreSQL 'Box' from corners (lowerX, lowerY, upperX, upperY) with validation.
-- Returns 'Nothing' if lowerX > upperX or lowerY > upperY.
refineFromCorners :: Double -> Double -> Double -> Double -> Maybe Box
refineFromCorners x1 y1 x2 y2 =
  if x1 <= x2 && y1 <= y2
    then Just (Box x1 y1 x2 y2)
    else Nothing
