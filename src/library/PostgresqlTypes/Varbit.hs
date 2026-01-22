module PostgresqlTypes.Varbit
  ( Varbit,

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
import Data.Hashable (Hashable (..))
import qualified Data.Text as Text
import qualified Data.Vector.Generic as Vg
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
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-bit.html).
--
-- The type parameter @maxLen@ specifies the static maximum length of the bit string.
-- Bit strings up to this length can be represented by this type.
data Varbit (maxLen :: TypeLits.Nat)
  = Varbit
      -- | Actual number of bits
      Int32
      -- | Bit data (packed into bytes)
      ByteString
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar (Varbit maxLen))

instance (TypeLits.KnownNat maxLen) => Arbitrary (Varbit maxLen) where
  arbitrary = do
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
    len <- QuickCheck.chooseInt (0, maxLen) -- Variable length up to max
    bits <- QuickCheck.vectorOf len (arbitrary :: QuickCheck.Gen Bool)
    case refineFromBoolList bits of
      Nothing -> error "Arbitrary Varbit: Generated bit string exceeds maximum length"
      Just varbit -> pure varbit
  shrink varbit =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
        bits = toBoolList varbit
        shrunkBitsList = shrink bits
     in mapMaybe refineFromBoolList [b | b <- shrunkBitsList, length b <= maxLen]

instance Hashable (Varbit maxLen) where
  hashWithSalt salt (Varbit len bytes) = salt `hashWithSalt` len `hashWithSalt` bytes

instance (TypeLits.KnownNat maxLen) => IsScalar (Varbit maxLen) where
  typeName = Tagged "varbit"
  baseOid = Tagged (Just 1562)
  arrayOid = Tagged (Just 1563)
  typeParams =
    Tagged [Text.pack (show (TypeLits.natVal (Proxy @maxLen)))]
  binaryEncoder (Varbit len bytes) =
    Write.bInt32 len <> Write.byteString bytes
  binaryDecoder =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
     in do
          len <- PtrPeeker.fixed PtrPeeker.beSignedInt4
          bytes <- PtrPeeker.remainderAsByteString

          pure
            if len <= maxLen
              then Right (Varbit len bytes)
              else
                Left
                  ( DecodingError
                      { location = ["Varbit"],
                        reason =
                          UnsupportedValueDecodingErrorReason
                            ("Varbit length " <> Text.pack (show len) <> " exceeds maximum " <> Text.pack (show maxLen))
                            (TextBuilder.toText (TextBuilder.decimal len))
                      }
                  )
  textualEncoder (Varbit len bytes) =
    let bits = concatMap byteToBits (ByteString.unpack bytes)
        trimmedBits = take (fromIntegral len) bits
        bitString = map (\b -> if b then '1' else '0') trimmedBits
     in TextBuilder.text (Text.pack bitString)
    where
      byteToBits :: Word8 -> [Bool]
      byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]
  textualDecoder =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
     in do
          bitText <- Attoparsec.takeWhile (\c -> c == '0' || c == '1')
          let len = Text.length bitText
          when (len > maxLen) do
            fail ("Varbit length " <> show len <> " exceeds maximum " <> show maxLen)
          let boolList = map (== '1') (Text.unpack bitText)
              numBytes = (len + 7) `div` 8
              paddedBoolList = boolList ++ replicate (numBytes * 8 - len) False
              bytes = map boolsToByte (chunksOf 8 paddedBoolList)
          pure (Varbit (fromIntegral len) (ByteString.pack bytes))
    where
      boolsToByte :: [Bool] -> Word8
      boolsToByte bs = foldl (\acc (i, b) -> if b then Bits.setBit acc i else acc) 0 (zip [7, 6, 5, 4, 3, 2, 1, 0] bs)
      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- * Accessors

-- | Extract the bit string as a list of Bool.
toBoolList :: forall maxLen. (TypeLits.KnownNat maxLen) => Varbit maxLen -> [Bool]
toBoolList (Varbit len bytes) =
  let bits = concatMap byteToBits (ByteString.unpack bytes)
      trimmedBits = take (fromIntegral len) bits
   in trimmedBits
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- | Extract the bit string as an unboxed vector of Bool.
toBoolVector :: forall maxLen vec. (TypeLits.KnownNat maxLen, Vg.Vector vec Bool) => Varbit maxLen -> vec Bool
toBoolVector (Varbit len bytes) =
  let bits = concatMap byteToBits (ByteString.unpack bytes)
      trimmedBits = take (fromIntegral len) bits
   in Vg.fromList trimmedBits
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = [Bits.testBit byte i | i <- [7, 6, 5, 4, 3, 2, 1, 0]]

-- * Constructors

-- | Construct a PostgreSQL 'Varbit' from a list of Bool with validation.
-- Returns 'Nothing' if the list length exceeds the maximum length.
refineFromBoolList :: forall maxLen. (TypeLits.KnownNat maxLen) => [Bool] -> Maybe (Varbit maxLen)
refineFromBoolList bits =
  let len = length bits
      maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
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

-- | Construct a PostgreSQL 'Varbit' from a list of Bool.
-- Truncates to the maximum length if necessary.
normalizeFromBoolList :: forall maxLen. (TypeLits.KnownNat maxLen) => [Bool] -> Varbit maxLen
normalizeFromBoolList bits =
  let maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
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

-- | Construct a PostgreSQL 'Varbit' from an unboxed vector of Bool with validation.
-- Returns 'Nothing' if the vector length exceeds the maximum length.
refineFromBoolVector :: forall maxLen vec. (TypeLits.KnownNat maxLen, Vg.Vector vec Bool) => vec Bool -> Maybe (Varbit maxLen)
refineFromBoolVector bitVector =
  let bits = Vg.toList bitVector
      len = fromIntegral (Vg.length bitVector)
      maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
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

-- | Construct a PostgreSQL 'Varbit' from an unboxed vector of Bool.
-- Truncates to the maximum length if necessary.
normalizeFromBoolVector :: forall maxLen vec. (TypeLits.KnownNat maxLen, Vg.Vector vec Bool) => vec Bool -> Varbit maxLen
normalizeFromBoolVector bitVector =
  let maxLen = fromIntegral (TypeLits.natVal (Proxy @maxLen))
      bits = Vg.toList bitVector
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
