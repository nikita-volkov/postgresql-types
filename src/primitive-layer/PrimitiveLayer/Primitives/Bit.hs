module PrimitiveLayer.Primitives.Bit (Bit) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector.Unboxed as VU
import qualified LawfulConversions
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @bit@ type. Fixed-length bit string.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-bit.html)
data Bit = Bit
  { -- | Number of bits
    bitLength :: Int32,
    -- | Bit data (packed into bytes)
    bitData :: ByteString
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Bit)

instance Arbitrary Bit where
  arbitrary = do
    len <- QuickCheck.chooseInt (0, 64) -- Reasonable bit length for tests
    bits <- QuickCheck.vectorOf len (arbitrary :: QuickCheck.Gen Bool) -- Generate the actual bits
    -- Convert through IsMany to ensure proper padding
    pure $ LawfulConversions.from bits
  shrink (Bit len bytes) =
    let bits = LawfulConversions.from (Bit len bytes) :: [Bool]
        shrunkBitsList = shrink bits
     in map LawfulConversions.from shrunkBitsList

instance Primitive Bit where
  typeName = Tagged "bit"
  baseOid = Tagged 1560
  arrayOid = Tagged 1561
  binaryEncoder (Bit len bytes) =
    mconcat
      [ Write.bInt32 len,
        Write.byteString bytes
      ]
  binaryDecoder = do
    len <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
    bytes <- PeekyBlinders.remainderAsByteString
    pure (Right (Bit len bytes))
  textualEncoder (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
        bitString = map (\b -> if b then '1' else '0') trimmedBits
     in TextBuilder.text (Text.pack bitString)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Convert from a bit string (as a list of Bool) to a Bit.
-- The bit string is packed into bytes.
instance IsSome [Bool] Bit where
  to (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  maybeFrom bits =
    let len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Just (Bit (fromIntegral len) (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert from a Bit to a list of Bool.
-- Only returns the actual bits (not padding).
instance IsSome Bit [Bool] where
  to bits =
    let len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Bit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  maybeFrom (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in Just trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Direct conversion from bit list to Bit.
instance IsMany [Bool] Bit where
  from bits =
    let len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Bit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from Bit to bit list.
instance IsMany Bit [Bool] where
  from (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Bidirectional conversion between bit list and Bit.
instance Is [Bool] Bit

instance Is Bit [Bool]

-- | Convert from an unboxed vector of Bool to a Bit.
--
-- This provides an efficient conversion from 'Data.Vector.Unboxed.Vector' 'Bool'
-- to PostgreSQL @bit@ type. The boolean vector is packed into bytes with proper
-- padding to align to byte boundaries.
--
-- This instance allows using unboxed vectors for high-performance bit operations
-- while maintaining compatibility with PostgreSQL's bit string format.
instance IsSome (VU.Vector Bool) Bit where
  to (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in VU.fromList trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  maybeFrom bitVector =
    let bits = VU.toList bitVector
        len = fromIntegral (VU.length bitVector)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Just (Bit (fromIntegral len) (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert from a Bit to an unboxed vector of Bool.
--
-- This provides an efficient conversion from PostgreSQL @bit@ type to
-- 'Data.Vector.Unboxed.Vector' 'Bool'. Only returns the actual bits,
-- excluding any padding bits used for byte alignment.
--
-- This is the inverse of the 'IsSome' instance for @(VU.Vector Bool) Bit@.
instance IsSome Bit (VU.Vector Bool) where
  to bitVector =
    let bits = VU.toList bitVector
        len = fromIntegral (VU.length bitVector)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Bit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  maybeFrom (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in Just (VU.fromList trimmedBits)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Direct conversion from unboxed bit vector to Bit.
--
-- This is a total conversion that always succeeds. The boolean vector
-- is efficiently packed into the PostgreSQL @bit@ format.
instance IsMany (VU.Vector Bool) Bit where
  from bitVector =
    let bits = VU.toList bitVector
        len = fromIntegral (VU.length bitVector)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Bit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from Bit to unboxed bit vector.
--
-- This is a total conversion that always succeeds. Efficiently extracts
-- the bit data from PostgreSQL @bit@ format into an unboxed vector.
instance IsMany Bit (VU.Vector Bool) where
  from (Bit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in VU.fromList trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Bidirectional conversion between unboxed bit vector and Bit.
--
-- This provides isomorphic conversion between 'Data.Vector.Unboxed.Vector' 'Bool'
-- and PostgreSQL @bit@ type. These instances enable seamless use of unboxed vectors
-- for efficient bit manipulation while maintaining PostgreSQL compatibility.
instance Is (VU.Vector Bool) Bit

instance Is Bit (VU.Vector Bool)
