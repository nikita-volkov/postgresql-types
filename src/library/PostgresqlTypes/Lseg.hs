{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Lseg
  ( Lseg (..),

    -- * Accessors
    toEndpoints,

    -- * Constructors
    fromEndpoints,
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

-- | PostgreSQL @lseg@ type. Line segment in 2D plane.
--
-- The line segment is defined by two endpoints, each with (@x@,@y@) coordinates.
-- Stored as four @64@-bit floating point numbers: (@x1@, @y1@, @x2@, @y2@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-geometric.html#DATATYPE-LSEG).
data Lseg = Lseg
  { lsegX1 :: Double,
    lsegY1 :: Double,
    lsegX2 :: Double,
    lsegY2 :: Double
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Lseg)

instance Arbitrary Lseg where
  arbitrary = Lseg <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Lseg x1 y1 x2 y2) =
    [Lseg x1' y1' x2' y2' | (x1', y1', x2', y2') <- shrink (x1, y1, x2, y2)]

instance IsScalar Lseg where
  typeName = Tagged "lseg"
  baseOid = Tagged (Just 601)
  arrayOid = Tagged (Just 1018)
  typeParams = Tagged []
  binaryEncoder (Lseg x1 y1 x2 y2) =
    mconcat
      [ Write.bWord64 (castDoubleToWord64 x1),
        Write.bWord64 (castDoubleToWord64 y1),
        Write.bWord64 (castDoubleToWord64 x2),
        Write.bWord64 (castDoubleToWord64 y2)
      ]
  binaryDecoder = do
    x1 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    y1 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    x2 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    y2 <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    pure (Right (Lseg x1 y1 x2 y2))
  textualEncoder (Lseg x1 y1 x2 y2) =
    "[("
      <> TextBuilder.string (printf "%g" x1)
      <> ","
      <> TextBuilder.string (printf "%g" y1)
      <> "),"
      <> "("
      <> TextBuilder.string (printf "%g" x2)
      <> ","
      <> TextBuilder.string (printf "%g" y2)
      <> ")]"
  textualDecoder = do
    _ <- Attoparsec.char '['
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
    _ <- Attoparsec.char ']'
    pure (Lseg x1 y1 x2 y2)

-- * Accessors

-- | Extract the endpoints as a 4-tuple (x1, y1, x2, y2).
toEndpoints :: Lseg -> (Double, Double, Double, Double)
toEndpoints (Lseg x1 y1 x2 y2) = (x1, y1, x2, y2)

-- * Constructors

-- | Construct a PostgreSQL 'Lseg' from endpoints (x1, y1, x2, y2).
fromEndpoints :: (Double, Double, Double, Double) -> Lseg
fromEndpoints (x1, y1, x2, y2) = Lseg x1 y1 x2 y2
