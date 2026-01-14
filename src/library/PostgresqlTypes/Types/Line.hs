{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlTypes.Types.Line (Line) where

import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @line@ type. Infinite line in 2D plane.
--
-- The line is represented by the linear equation @Ax + By + C = 0@.
-- Stored as three @64@-bit floating point numbers (@A@, @B@, @C@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-geometric.html#DATATYPE-LINE).
data Line = Line
  { lineA :: Double,
    lineB :: Double,
    lineC :: Double
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Line)

instance Arbitrary Line where
  arbitrary = do
    -- Ensure at least one of A or B is non-zero
    (a, b) <-
      QuickCheck.suchThat
        ((,) <$> arbitrary <*> arbitrary)
        (\(a, b) -> not (a == 0 && b == 0))
    c <- arbitrary
    pure (Line a b c)
  shrink (Line a b c) =
    [ Line a' b' c'
    | (a', b', c') <- shrink (a, b, c),
      not (a' == 0 && b' == 0) -- Ensure shrunk values are also valid
    ]

instance IsStandardType Line where
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
    a <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    b <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
    c <- PtrPeeker.fixed (castWord64ToDouble <$> PtrPeeker.beUnsignedInt8)
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
  maybeFrom (a, b, c) = do
    when (a == 0 && b == 0) empty
    pure (Line a b c)

-- | Direct conversion from tuple to Line.
--
-- Defaults to vertical line, when A and B equal 0.
instance IsMany (Double, Double, Double) Line where
  onfrom (a, b, c) =
    if a == 0 && b == 0
      then Line 1 0 c
      else Line a b c
