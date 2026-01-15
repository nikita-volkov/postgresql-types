{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Path (Path) where

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

-- | PostgreSQL @path@ type. Geometric path in 2D plane (open or closed).
--
-- Represented as a series of connected points, which can be either open or closed.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-geometric.html#DATATYPE-PATH).
data Path = Path
  { closed :: Bool,
    points :: UnboxedVector.Vector (Double, Double)
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Path)

instance Arbitrary Path where
  arbitrary = do
    closed <- arbitrary
    size <- QuickCheck.getSize
    -- Paths need at least 1 point
    numPoints <- QuickCheck.chooseInt (1, max 1 size)
    points <- UnboxedVector.replicateM numPoints arbitrary
    pure (Path closed points)
  shrink (Path closed points) =
    [ Path closed' points'
    | (closed', points') <- shrink (closed, points),
      UnboxedVector.length points' >= 1
    ]

instance IsStandardType Path where
  typeName = Tagged "path"
  baseOid = Tagged (Just 602)
  arrayOid = Tagged (Just 1019)
  binaryEncoder (Path closed points) =
    let closedByte = if closed then 1 else 0 :: Word8
        numPoints = fromIntegral (UnboxedVector.length points) :: Int32
        pointsEncoded = UnboxedVector.foldMap encodePoint points
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
    (closedByte, numPoints) <- PtrPeeker.fixed do
      (,) <$> PtrPeeker.unsignedInt1 <*> PtrPeeker.beSignedInt4
    points <- UnboxedVector.replicateM (fromIntegral numPoints) decodePoint
    let closed = closedByte /= 0
    pure (Right (Path closed points))
    where
      decodePoint = PtrPeeker.fixed do
        x <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
        y <- castWord64ToDouble <$> PtrPeeker.beUnsignedInt8
        pure (x, y)
  textualEncoder (Path closed points) =
    let openChar = if closed then "(" else "["
        closeChar = if closed then ")" else "]"
        pointsStr = TextBuilder.intercalateMap "," encodePoint (UnboxedVector.toList points)
     in openChar <> pointsStr <> closeChar
    where
      encodePoint (x, y) =
        "(" <> TextBuilder.string (printf "%g" x) <> "," <> TextBuilder.string (printf "%g" y) <> ")"
  textualDecoder = do
    closed <-
      True
        <$ Attoparsec.char '('
        <|> False
        <$ Attoparsec.char '['
    points <- parsePoint `Attoparsec.sepBy1` Attoparsec.char ','
    _ <- Attoparsec.char (if closed then ')' else ']')
    pure (Path closed (UnboxedVector.fromList points))
    where
      parsePoint = do
        _ <- Attoparsec.char '('
        x <- Attoparsec.double
        _ <- Attoparsec.char ','
        y <- Attoparsec.double
        _ <- Attoparsec.char ')'
        pure (x, y)

-- | Convert from a tuple of Bool and list of points to a Path.
-- This is always safe since both represent the same data.
instance IsSome (Bool, [(Double, Double)]) Path where
  to (Path closed points) = (closed, UnboxedVector.toList points)
  maybeFrom (closed, points) =
    case points of
      [] -> Nothing
      _ -> Just (Path closed (UnboxedVector.fromList points))

-- | Convert from a tuple of Bool and list of points to a Path.
-- This is always safe since both represent the same data.
instance IsSome (Bool, (UnboxedVector.Vector (Double, Double))) Path where
  to (Path closed points) = (closed, points)
  maybeFrom (closed, points) =
    if UnboxedVector.length points >= 1
      then Just (Path closed points)
      else Nothing
