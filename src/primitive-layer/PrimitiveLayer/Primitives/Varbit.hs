-- | PostgreSQL @varbit@ type.
-- Represents variable-length bit strings in PostgreSQL.
module PrimitiveLayer.Primitives.Varbit (Varbit (..)) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified LawfulConversions
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @varbit@ type representing a variable-length bit string.
-- Similar to @bit@ but without a fixed maximum length.
-- Stored as a length (Int32) followed by the bit data in bytes.
data Varbit = Varbit
  { -- | Number of bits
    varbitLength :: Int32,
    -- | Bit data (packed into bytes)
    varbitData :: ByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Varbit)

instance Arbitrary Varbit where
  arbitrary = do
    len <- QuickCheck.chooseInt (0, 128) -- Reasonable bit length for tests
    bits <- QuickCheck.vectorOf len (arbitrary :: QuickCheck.Gen Bool) -- Generate the actual bits
    -- Convert through IsMany to ensure proper padding
    pure $ LawfulConversions.from bits
  shrink (Varbit len bytes) =
    let bits = LawfulConversions.from (Varbit len bytes) :: [Bool]
        shrunkBitsList = shrink bits
     in map LawfulConversions.from shrunkBitsList

instance Primitive Varbit where
  typeName = Tagged "varbit"
  baseOid = Tagged 1562
  arrayOid = Tagged 1563
  binaryEncoder (Varbit len bytes) =
    mconcat
      [ Write.bInt32 len,
        Write.byteString bytes
      ]
  binaryDecoder = do
    len <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
    bytes <- PeekyBlinders.remainderAsByteString
    pure (Right (Varbit len bytes))
  textualEncoder (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
        bitString = map (\b -> if b then '1' else '0') trimmedBits
     in TextBuilder.text (Text.pack bitString)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Convert from a bit string (as a list of Bool) to a Varbit.
-- The bit string is packed into bytes.
instance IsSome [Bool] Varbit where
  to (Varbit len bytes) =
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
     in Just (Varbit (fromIntegral len) (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert from a Varbit to a list of Bool.
-- Only returns the actual bits (not padding).
instance IsSome Varbit [Bool] where
  to bits =
    let len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  maybeFrom (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in Just trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Direct conversion from bit list to Varbit.
instance IsMany [Bool] Varbit where
  from bits =
    let len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf n [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from Varbit to bit list.
instance IsMany Varbit [Bool] where
  from (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Bidirectional conversion between bit list and Varbit.
instance Is [Bool] Varbit

instance Is Varbit [Bool]
