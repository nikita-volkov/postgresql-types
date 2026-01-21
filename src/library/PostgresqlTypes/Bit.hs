{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Bit
  ( Bit,

    -- * Accessors
    toBoolList,
    toBoolVector,

    -- * Constructors
    refineFromBoolList,
    normalizeFromBoolList,
    refineFromBoolVector,
    normalizeFromBoolVector,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as VU
import qualified GHC.TypeLits as TypeLits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @bit@ type. Fixed-length bit string.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-bit.html).
--
-- The type parameter @numBits@ specifies the static length of the bit string.
-- Only bit strings with exactly this length can be represented by this type.
data Bit (numBits :: TypeLits.Nat) = Bit
  { -- | Bit data (packed into bytes)
    bytes :: ByteString
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar (Bit numBits))

instance (TypeLits.KnownNat numBits) => Arbitrary (Bit numBits) where
  arbitrary = do
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
    boolList <- QuickCheck.vectorOf len (arbitrary @Bool)
    case refineFromBoolList boolList of
      Nothing -> error "Arbitrary Bit: Generated bit string has incorrect length"
      Just bit -> pure bit

instance (TypeLits.KnownNat numBits) => IsScalar (Bit numBits) where
  typeName = Tagged "bit"
  baseOid = Tagged (Just 1560)
  arrayOid = Tagged (Just 1561)
  typeParams =
    Tagged
      [ Text.pack (show (TypeLits.natVal (Proxy @numBits)))
      ]
  binaryEncoder (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
     in mconcat
          [ Write.bInt32 len,
            Write.byteString bytes
          ]
  binaryDecoder = do
    len <- PtrPeeker.fixed PtrPeeker.beSignedInt4
    bytes <- PtrPeeker.remainderAsByteString
    let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    if len == expectedLen
      then pure (Right (Bit bytes))
      else
        pure
          ( Left
              ( DecodingError
                  { location = ["Bit"],
                    reason =
                      UnsupportedValueDecodingErrorReason
                        ("Expected bit string of length " <> Text.pack (show expectedLen) <> " but got " <> Text.pack (show len))
                        (TextBuilder.toText (TextBuilder.decimal len))
                  }
              )
          )
  textualEncoder (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
        bitString = map (\b -> if b then '1' else '0') trimmedBits
     in TextBuilder.text (Text.pack bitString)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  textualDecoder = do
    bitChars <- Attoparsec.takeText
    let bits = map (== '1') (Text.unpack bitChars)
        len = length bits
        expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    if len /= expectedLen
      then fail ("Expected bit string of length " <> show expectedLen <> " but got " <> show len)
      else do
        let numBytes = (len + 7) `div` 8
            paddedBits = bits ++ replicate (numBytes * 8 - len) False
            bytes = map boolsToByte (chunksOf 8 paddedBits)
        pure (Bit (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- * Accessors

-- | Extract the bit string as a list of Bool.
toBoolList :: forall numBits. (TypeLits.KnownNat numBits) => Bit numBits -> [Bool]
toBoolList (Bit bytes) =
  let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
      bits = concatMap byteToBits (ByteString.unpack bytes)
      trimmedBits = take len bits
   in trimmedBits
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Extract the bit string as an unboxed vector of Bool.
toBoolVector :: forall numBits. (TypeLits.KnownNat numBits) => Bit numBits -> VU.Vector Bool
toBoolVector (Bit bytes) =
  let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
      bits = concatMap byteToBits (ByteString.unpack bytes)
      trimmedBits = take len bits
   in VU.fromList trimmedBits
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- * Constructors

-- | Construct a PostgreSQL 'Bit' from a list of Bool with validation.
-- Returns 'Nothing' if the list length doesn't match the expected length.
refineFromBoolList :: forall numBits. (TypeLits.KnownNat numBits) => [Bool] -> Maybe (Bit numBits)
refineFromBoolList bits =
  let len = length bits
      expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
   in if len == expectedLen
        then
          let numBytes = (len + 7) `div` 8
              paddedBits = bits ++ replicate (numBytes * 8 - len) False
              bytes = map boolsToByte (chunksOf 8 paddedBits)
           in Just (Bit (ByteString.pack bytes))
        else Nothing
  where
    boolsToByte :: [Bool] -> Word8
    boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Construct a PostgreSQL 'Bit' from a list of Bool.
-- Truncates or pads to match the type-level length.
normalizeFromBoolList :: forall numBits. (TypeLits.KnownNat numBits) => [Bool] -> Bit numBits
normalizeFromBoolList bits =
  let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
      -- Truncate or pad to the expected length
      adjustedBits = take expectedLen (bits ++ repeat False)
      numBytes = (expectedLen + 7) `div` 8
      paddedBits = adjustedBits ++ replicate (numBytes * 8 - expectedLen) False
      bytes = map boolsToByte (chunksOf 8 paddedBits)
   in Bit (ByteString.pack bytes)
  where
    boolsToByte :: [Bool] -> Word8
    boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Construct a PostgreSQL 'Bit' from an unboxed vector of Bool with validation.
-- Returns 'Nothing' if the vector length doesn't match the expected length.
refineFromBoolVector :: forall numBits. (TypeLits.KnownNat numBits) => VU.Vector Bool -> Maybe (Bit numBits)
refineFromBoolVector bitVector =
  let len = VU.length bitVector
      expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
   in if len == expectedLen
        then
          let bits = VU.toList bitVector
              numBytes = (len + 7) `div` 8
              paddedBits = bits ++ replicate (numBytes * 8 - len) False
              bytes = map boolsToByte (chunksOf 8 paddedBits)
           in Just (Bit (ByteString.pack bytes))
        else Nothing
  where
    boolsToByte :: [Bool] -> Word8
    boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Construct a PostgreSQL 'Bit' from an unboxed vector of Bool.
-- Truncates or pads to match the type-level length.
normalizeFromBoolVector :: forall numBits. (TypeLits.KnownNat numBits) => VU.Vector Bool -> Bit numBits
normalizeFromBoolVector bitVector =
  let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
      bits = VU.toList bitVector
      -- Truncate or pad to the expected length
      adjustedBits = take expectedLen (bits ++ repeat False)
      numBytes = (expectedLen + 7) `div` 8
      paddedBits = adjustedBits ++ replicate (numBytes * 8 - expectedLen) False
      bytes = map boolsToByte (chunksOf 8 paddedBits)
   in Bit (ByteString.pack bytes)
  where
    boolsToByte :: [Bool] -> Word8
    boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
