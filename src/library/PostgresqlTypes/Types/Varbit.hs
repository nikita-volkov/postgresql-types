{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Varbit (Varbit) where

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

-- | PostgreSQL @varbit(n)@ type. Variable-length bit string with limit.
--
-- Similar to @bit@ but with a variable length up to the specified maximum.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-bit.html).
--
-- The type parameter @numBits@ specifies the static maximum length of the bit string.
-- Bit strings up to this length can be represented by this type.
data Varbit (numBits :: TypeLits.Nat) = Varbit
  { -- | Number of bits
    varbitLength :: Int32,
    -- | Bit data (packed into bytes)
    varbitData :: ByteString
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Varbit numBits))

instance (TypeLits.KnownNat numBits) => Arbitrary (Varbit numBits) where
  arbitrary = do
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    len <- QuickCheck.chooseInt (0, maxLen) -- Variable length up to max
    bits <- QuickCheck.vectorOf len (arbitrary :: QuickCheck.Gen Bool)
    case maybeFrom bits of
      Nothing -> error "Arbitrary Varbit: Generated bit string exceeds maximum length"
      Just varbit -> pure varbit
  shrink varbit =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = to @[Bool] varbit
        shrunkBitsList = shrink bits
     in mapMaybe maybeFrom [b | b <- shrunkBitsList, length b <= maxLen]

instance (TypeLits.KnownNat numBits) => IsStandardType (Varbit numBits) where
  typeName = Tagged "varbit"
  baseOid = Tagged (Just 1562)
  arrayOid = Tagged (Just 1563)
  typeParams =
    Tagged [Text.pack (show (TypeLits.natVal (Proxy @numBits)))]
  binaryEncoder (Varbit len bytes) =
    mconcat
      [ Write.bInt32 len,
        Write.byteString bytes
      ]
  binaryDecoder = do
    len <- PtrPeeker.fixed PtrPeeker.beSignedInt4
    bytes <- PtrPeeker.remainderAsByteString
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    if len <= maxLen
      then pure (Right (Varbit len bytes))
      else
        pure
          ( Left
              ( DecodingError
                  { location = ["Varbit"],
                    reason =
                      UnsupportedValueDecodingErrorReason
                        ("Varbit length " <> Text.pack (show len) <> " exceeds maximum " <> Text.pack (show maxLen))
                        (TextBuilder.toText (TextBuilder.decimal len))
                  }
              )
          )
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
        maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
    if len <= maxLen
      then do
        let numBytes = (len + 7) `div` 8
            paddedBits = bits ++ replicate (numBytes * 8 - len) False
            bytes = map boolsToByte (chunksOf 8 paddedBits)
        pure (Varbit (fromIntegral len) (ByteString.pack bytes))
      else fail ("Varbit length " <> show len <> " exceeds maximum " <> show maxLen)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert from a bit string (as a list of Bool) to a Varbit.
-- The bit string must not exceed the maximum length specified by the type parameter.
-- The bit string is packed into bytes.
instance (TypeLits.KnownNat numBits) => IsSome [Bool] (Varbit numBits) where
  to (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
     in trimmedBits
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  maybeFrom bits =
    let len = length bits
        maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
     in if len <= maxLen
          then
            let numBytes = (len + 7) `div` 8
                paddedBits = bits ++ replicate (numBytes * 8 - len) False
                bytes = map boolsToByte (chunksOf 8 paddedBits)
             in Just (Varbit (fromIntegral len) (ByteString.pack bytes))
          else Nothing
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from bit list to Varbit.
-- Truncates to the maximum length if necessary.
instance (TypeLits.KnownNat numBits) => IsMany [Bool] (Varbit numBits) where
  onfrom bits =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
        truncatedBits = take maxLen bits
        actualLen = length truncatedBits
        numBytes = (actualLen + 7) `div` 8
        paddedBits = truncatedBits ++ replicate (numBytes * 8 - actualLen) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral actualLen) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert from an unboxed vector of Bool to a Varbit.
--
-- This provides an efficient conversion from 'Data.Vector.Unboxed.Vector' 'Bool'
-- to PostgreSQL @varbit@ type. The boolean vector is packed into bytes with proper
-- padding to align to byte boundaries.
--
-- This instance allows using unboxed vectors for high-performance bit operations
-- while maintaining compatibility with PostgreSQL's variable-length bit string format.
instance (TypeLits.KnownNat numBits) => IsSome (VU.Vector Bool) (Varbit numBits) where
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
        maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
     in if len <= maxLen
          then
            let numBytes = (len + 7) `div` 8
                paddedBits = bits ++ replicate (numBytes * 8 - len) False
                bytes = map boolsToByte (chunksOf 8 paddedBits)
             in Just (Varbit (fromIntegral len) (ByteString.pack bytes))
          else Nothing
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Direct conversion from unboxed bit vector to Varbit.
-- Truncates to the maximum length if necessary.
--
-- This is a total conversion that always succeeds. The boolean vector
-- is efficiently packed into the PostgreSQL @varbit@ format.
instance (TypeLits.KnownNat numBits) => IsMany (VU.Vector Bool) (Varbit numBits) where
  onfrom bitVector =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numBits))
        bits = VU.toList bitVector
        truncatedBits = take maxLen bits
        actualLen = length truncatedBits
        numBytes = (actualLen + 7) `div` 8
        paddedBits = truncatedBits ++ replicate (numBytes * 8 - actualLen) False
        bytes = map boolsToByte (chunksOf 8 paddedBits)
     in Varbit (fromIntegral actualLen) (ByteString.pack bytes)
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)
