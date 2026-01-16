{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Bit (Bit) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as VU
import qualified GHC.TypeLits as TypeLits
import qualified LawfulConversions
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @bit@ type. Fixed-length bit string.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-bit.html).
-- 
-- The type parameter @numBits@ specifies the static length of the bit string.
-- Only bit strings with exactly this length can be represented by this type.
data Bit (numBits :: TypeLits.Nat) = Bit
  { -- | Bit data (packed into bytes)
    bytes :: ByteString
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Bit numBits))

instance (TypeLits.KnownNat numBits) => Arbitrary (Bit numBits) where
  arbitrary = do
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
    bits <- QuickCheck.vectorOf len (arbitrary :: QuickCheck.Gen Bool)
    -- Convert through IsMany to ensure proper padding
    pure $ LawfulConversions.from bits
  shrink (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = LawfulConversions.from (Bit @numBits bytes) :: [Bool]
        shrunkBitsList = filter (\xs -> length xs == len) (shrink bits)
     in map LawfulConversions.from shrunkBitsList

instance (TypeLits.KnownNat numBits) => IsStandardType (Bit numBits) where
  typeName = Tagged "bit"
  baseOid = Tagged (Just 1560)
  arrayOid = Tagged (Just 1561)
  typeParams = Tagged
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
      else pure (Left (DecodingError 
        { location = ["Bit"]
        , reason = UnsupportedValueDecodingErrorReason
            ("Expected bit string of length " <> Text.pack (show expectedLen) <> " but got " <> Text.pack (show len))
            (TextBuilder.toText (TextBuilder.decimal len))
        }))
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

-- | Convert from a bit string (as a list of Bool) to a Bit.
-- The bit string must have exactly the length specified by the type parameter.
-- The bit string is packed into bytes.
instance (TypeLits.KnownNat numBits) => IsSome [Bool] (Bit numBits) where
  to (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  maybeFrom bits =
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

-- | Convert from a Bit to a list of Bool.
-- Only returns the actual bits (not padding).
instance (TypeLits.KnownNat numBits) => IsSome (Bit numBits) [Bool] where
  to bits =
    let len = length bits
        expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    in if len == expectedLen
      then
        let numBytes = (len + 7) `div` 8
            paddedBits = bits ++ replicate (numBytes * 8 - len) False
            bytes = map boolsToByte (chunksOf 8 paddedBits)
         in Bit (ByteString.pack bytes)
      else Bit ByteString.empty -- Invalid, but needed for type signature
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  maybeFrom (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
     in if length trimmedBits == len
          then Just trimmedBits
          else Nothing
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Direct conversion from bit list to Bit.
-- Truncates or pads to match the type-level length.
instance (TypeLits.KnownNat numBits) => IsMany [Bool] (Bit numBits) where
  onfrom bits =
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

-- | Direct conversion from Bit to bit list.
instance (TypeLits.KnownNat numBits) => IsMany (Bit numBits) [Bool] where
  onfrom (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Convert from an unboxed vector of Bool to a Bit.
--
-- This provides an efficient conversion from 'Data.Vector.Unboxed.Vector' 'Bool'
-- to PostgreSQL @bit@ type. The boolean vector must have exactly the length specified
-- by the type parameter. The boolean vector is packed into bytes with proper
-- padding to align to byte boundaries.
--
-- This instance allows using unboxed vectors for high-performance bit operations
-- while maintaining compatibility with PostgreSQL's bit string format.
instance (TypeLits.KnownNat numBits) => IsSome (VU.Vector Bool) (Bit numBits) where
  to (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
     in VU.fromList trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  maybeFrom bitVector =
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

-- | Convert from a Bit to an unboxed vector of Bool.
--
-- This provides an efficient conversion from PostgreSQL @bit@ type to
-- 'Data.Vector.Unboxed.Vector' 'Bool'. Only returns the actual bits,
-- excluding any padding bits used for byte alignment.
--
-- This is the inverse of the 'IsSome' instance for @(VU.Vector Bool) (Bit numBits)@.
instance (TypeLits.KnownNat numBits) => IsSome (Bit numBits) (VU.Vector Bool) where
  to bitVector =
    let len = VU.length bitVector
        expectedLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    in if len == expectedLen
      then
        let bits = VU.toList bitVector
            numBytes = (len + 7) `div` 8
            paddedBits = bits ++ replicate (numBytes * 8 - len) False
            bytes = map boolsToByte (chunksOf 8 paddedBits)
         in Bit (ByteString.pack bytes)
      else Bit ByteString.empty -- Invalid, but needed for type signature
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
  maybeFrom (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
     in if length trimmedBits == len
          then Just (VU.fromList trimmedBits)
          else Nothing
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Direct conversion from unboxed bit vector to Bit.
--
-- This is a total conversion that truncates or pads to match the type-level length.
-- The boolean vector is efficiently packed into the PostgreSQL @bit@ format.
instance (TypeLits.KnownNat numBits) => IsMany (VU.Vector Bool) (Bit numBits) where
  onfrom bitVector =
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

-- | Direct conversion from Bit to unboxed bit vector.
--
-- This is a total conversion that always succeeds. Efficiently extracts
-- the bit data from PostgreSQL @bit@ format into an unboxed vector.
instance (TypeLits.KnownNat numBits) => IsMany (Bit numBits) (VU.Vector Bool) where
  onfrom (Bit bytes) =
    let len = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take len bits
     in VU.fromList trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
