{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Varbit (Varbit) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as VU
import qualified LawfulConversions
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @varbit@ type. Variable-length bit string.
--
-- Similar to @bit@ but without a fixed maximum length.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-bit.html).
data Varbit = Varbit
  { -- | Number of bits
    varbitLength :: Int32,
    -- | Bit data (packed into bytes)
    varbitData :: ByteString
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Varbit)

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

instance IsStandardType Varbit where
  typeName = Tagged "varbit"
  baseOid = Tagged (Just 1562)
  arrayOid = Tagged (Just 1563)
  binaryEncoder (Varbit len bytes) =
    mconcat
      [ Write.bInt32 len,
        Write.byteString bytes
      ]
  binaryDecoder = do
    len <- PtrPeeker.fixed PtrPeeker.beSignedInt4
    bytes <- PtrPeeker.remainderAsByteString
    pure (Right (Varbit len bytes))
  textualEncoder (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
        bitString = map (\b -> if b then '1' else '0') trimmedBits
     in TextBuilder.text (Text.pack bitString)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  textualDecoder = do
    bitChars <- Attoparsec.takeText
    let bits = map (== '1') (Text.unpack bitChars)
        len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
    pure (Varbit (fromIntegral len) (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

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
      chunksOf _ [] = []
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
      chunksOf _ [] = []
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
  onfrom bits =
    let len = fromIntegral (length bits)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from Varbit to bit list.
instance IsMany Varbit [Bool] where
  onfrom (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Bidirectional conversion between bit list and Varbit.
instance Is [Bool] Varbit

instance Is Varbit [Bool]

-- | Convert from an unboxed vector of Bool to a Varbit.
--
-- This provides an efficient conversion from 'Data.Vector.Unboxed.Vector' 'Bool'
-- to PostgreSQL @varbit@ type. The boolean vector is packed into bytes with proper
-- padding to align to byte boundaries.
--
-- This instance allows using unboxed vectors for high-performance bit operations
-- while maintaining compatibility with PostgreSQL's variable-length bit string format.
instance IsSome (VU.Vector Bool) Varbit where
  to (Varbit len bytes) =
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
     in Just (Varbit (fromIntegral len) (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert from a Varbit to an unboxed vector of Bool.
--
-- This provides an efficient conversion from PostgreSQL @varbit@ type to
-- 'Data.Vector.Unboxed.Vector' 'Bool'. Only returns the actual bits,
-- excluding any padding bits used for byte alignment.
--
-- This is the inverse of the 'IsSome' instance for @(VU.Vector Bool) Varbit@.
instance IsSome Varbit (VU.Vector Bool) where
  to bitVector =
    let bits = VU.toList bitVector
        len = fromIntegral (VU.length bitVector)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  maybeFrom (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in Just (VU.fromList trimmedBits)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Direct conversion from unboxed bit vector to Varbit.
--
-- This is a total conversion that always succeeds. The boolean vector
-- is efficiently packed into the PostgreSQL @varbit@ format.
instance IsMany (VU.Vector Bool) Varbit where
  onfrom bitVector =
    let bits = VU.toList bitVector
        len = fromIntegral (VU.length bitVector)
        numBytes = (len + 7) `div` 8
        paddedBits = bits ++ replicate (numBytes * 8 - len) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral len) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from Varbit to unboxed bit vector.
--
-- This is a total conversion that always succeeds. Efficiently extracts
-- the bit data from PostgreSQL @varbit@ format into an unboxed vector.
instance IsMany Varbit (VU.Vector Bool) where
  onfrom (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in VU.fromList trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Bidirectional conversion between unboxed bit vector and Varbit.
--
-- This provides isomorphic conversion between 'Data.Vector.Unboxed.Vector' 'Bool'
-- and PostgreSQL @varbit@ type. These instances enable seamless use of unboxed vectors
-- for efficient bit manipulation while maintaining PostgreSQL compatibility.
instance Is (VU.Vector Bool) Varbit

instance Is Varbit (VU.Vector Bool)
