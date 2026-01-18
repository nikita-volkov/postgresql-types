{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Box (Box) where

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
data Box = Box
  { -- | Lower-left x coordinate
    boxX1 :: Double,
    -- | Lower-left y coordinate
    boxY1 :: Double,
    -- | Upper-right x coordinate
    boxX2 :: Double,
    -- | Upper-right y coordinate
    boxY2 :: Double
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Box)

instance Arbitrary Box where
  arbitrary = do
    x1 <- arbitrary
    y1 <- arbitrary
    x2 <- arbitrary
    y2 <- arbitrary
    -- Normalize the box so x1 <= x2 and y1 <= y2
    pure (Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))

  shrink (Box x1 y1 x2 y2) =
    [ Box (min x1' x2') (min y1' y2') (max x1' x2') (max y1' y2')
    | (x1', y1', x2', y2') <- shrink (x1, y1, x2, y2)
    ]

instance IsScalar Box where
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
    pure (Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))

-- | Mapping to a tuple of coordinates of lower-left and upper-right corners represented as @(lowerX, lowerY, upperX, upperY)@.
--
-- Input is validated to ensure @lowerX <= upperX@ and @lowerY <= upperY@.
instance IsSome (Double, Double, Double, Double) Box where
  to (Box x1 y1 x2 y2) = (x1, y1, x2, y2)
  maybeFrom (x1, y1, x2, y2) =
    if x1 <= x2 && y1 <= y2
      then Just (Box x1 y1 x2 y2)
      else Nothing

-- | Mapping to a tuple of coordinates of lower-left and upper-right corners represented as @(lowerX, lowerY, upperX, upperY)@.
--
-- Input is normalized to ensure @lowerX <= upperX@ and @lowerY <= upperY@.
instance IsMany (Double, Double, Double, Double) Box where
  onfrom (x1, y1, x2, y2) = Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
