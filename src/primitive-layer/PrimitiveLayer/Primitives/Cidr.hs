-- | PostgreSQL @cidr@ type.
-- Represents IPv4 or IPv6 network addresses (CIDR notation) in PostgreSQL.
module PrimitiveLayer.Primitives.Cidr (Cidr (..), CidrIpAddress (..)) where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Numeric (showHex)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import Test.QuickCheck (Gen, suchThat)
import qualified TextBuilder

-- | IP address type for representing IPv4 and IPv6 addresses.
data CidrIpAddress
  = -- | IPv4 address stored as 32-bit big-endian word
    V4CidrIpAddress Word32
  | -- | IPv6 address stored as four 32-bit big-endian words
    V6CidrIpAddress Word32 Word32 Word32 Word32
  deriving stock (Eq, Ord, Generic, Show)

-- | PostgreSQL @cidr@ type representing IPv4 or IPv6 network addresses.
-- Similar to inet but specifically for network addresses in CIDR notation.
-- In binary format, PostgreSQL stores:
-- - 1 byte: address family (IPv4=2, IPv6=3)
-- - 1 byte: netmask bits
-- - 1 byte: is_cidr flag (1 for cidr, 0 for inet)
-- - 1 byte: address length in bytes
-- - N bytes: address (4 for IPv4, 16 for IPv6)
data Cidr = Cidr
  { -- | Network address (host bits must be zero)
    cidrAddress :: CidrIpAddress,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    cidrNetmask :: Word8
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Cidr)

instance Arbitrary CidrIpAddress where
  arbitrary = do
    isIPv4 <- arbitrary :: Gen Bool
    if isIPv4
      then V4CidrIpAddress <$> arbitrary
      else V6CidrIpAddress <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (V4CidrIpAddress addr) = V4CidrIpAddress <$> shrink addr
  shrink (V6CidrIpAddress w1 w2 w3 w4) =
    [V6CidrIpAddress w1' w2' w3' w4' | (w1', w2', w3', w4') <- shrink (w1, w2, w3, w4)]

instance Arbitrary Cidr where
  arbitrary = do
    address <- arbitrary :: Gen CidrIpAddress
    netmask <- case address of
      V4CidrIpAddress _ -> arbitrary `suchThat` (<= 32) :: Gen Word8
      V6CidrIpAddress _ _ _ _ -> arbitrary `suchThat` (<= 128) :: Gen Word8
    pure (normalizeCidr address netmask)
  shrink (Cidr address netmask) =
    [ normalizeCidr address' netmask'
    | address' <- shrink address,
      netmask' <- shrink netmask,
      case address' of
        V4CidrIpAddress _ -> netmask' <= 32
        V6CidrIpAddress _ _ _ _ -> netmask' <= 128
    ]

instance Primitive Cidr where
  typeName = Tagged "cidr"
  baseOid = Tagged 650
  arrayOid = Tagged 651
  binaryEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4CidrIpAddress addr ->
        mconcat
          [ Write.word8 2, -- IPv4 address family
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 4, -- address length (4 bytes for IPv4)
            Write.bWord32 addr -- IPv4 address
          ]
      V6CidrIpAddress w1 w2 w3 w4 ->
        mconcat
          [ Write.word8 3, -- IPv6 address family for CIDR
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 16, -- address length (16 bytes for IPv6)
            Write.bWord32 w1,
            Write.bWord32 w2,
            Write.bWord32 w3,
            Write.bWord32 w4
          ]

  binaryDecoder = do
    (family, netmask, isCidrFlag, addrLen) <-
      PeekyBlinders.statically do
        (,,,)
          <$> PeekyBlinders.unsignedInt1
          <*> PeekyBlinders.unsignedInt1
          <*> PeekyBlinders.unsignedInt1
          <*> PeekyBlinders.unsignedInt1

    runExceptT do
      when (isCidrFlag /= 1) do
        throwError (DecodingError ["is-cidr"] (UnexpectedValueDecodingErrorReason "1" (TextBuilder.toText (TextBuilder.decimal isCidrFlag))))

      ip <- case family of
        2 -> do
          -- IPv4
          when (addrLen /= 4) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "4" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          addr <- lift do
            PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
          pure (V4CidrIpAddress addr)
        3 -> do
          -- IPv6
          when (addrLen /= 16) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          lift do
            PeekyBlinders.statically do
              V6CidrIpAddress
                <$> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
        _ -> do
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 3" (TextBuilder.toText (TextBuilder.decimal family))))

      pure (Cidr ip (fromIntegral netmask))

  textualEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4CidrIpAddress addr ->
        let a = fromIntegral ((addr `shiftR` 24) .&. 0xFF)
            b = fromIntegral ((addr `shiftR` 16) .&. 0xFF)
            c = fromIntegral ((addr `shiftR` 8) .&. 0xFF)
            d = fromIntegral (addr .&. 0xFF)
         in TextBuilder.string (show a)
              <> "."
              <> TextBuilder.string (show b)
              <> "."
              <> TextBuilder.string (show c)
              <> "."
              <> TextBuilder.string (show d)
              <> "/"
              <> TextBuilder.string (show netmask)
      V6CidrIpAddress w1 w2 w3 w4 ->
        -- Convert 32-bit words to proper IPv6 hex representation
        let toHex w =
              let h1 = fromIntegral ((w `shiftR` 16) .&. 0xFFFF) :: Word16
                  h2 = fromIntegral (w .&. 0xFFFF) :: Word16
               in TextBuilder.hexadecimal h1 <> ":" <> TextBuilder.hexadecimal h2
         in toHex w1
              <> ":"
              <> toHex w2
              <> ":"
              <> toHex w3
              <> ":"
              <> toHex w4
              <> "/"
              <> TextBuilder.decimal netmask

-- | Convert from (CidrIpAddress, Word8) to Cidr.
instance IsSome (CidrIpAddress, Word8) Cidr where
  to (Cidr addr netmask) = (addr, netmask)
  maybeFrom (addr, netmask) =
    case addr of
      V4CidrIpAddress _ -> if netmask <= 32 then Just (normalizeCidr addr netmask) else Nothing
      V6CidrIpAddress _ _ _ _ -> if netmask <= 128 then Just (normalizeCidr addr netmask) else Nothing

-- | Convert from Cidr to (CidrIpAddress, Word8).
instance IsSome Cidr (CidrIpAddress, Word8) where
  to (addr, netmask) = case addr of
    V4CidrIpAddress _ -> if netmask <= 32 then normalizeCidr addr netmask else error "Invalid IPv4 netmask"
    V6CidrIpAddress _ _ _ _ -> if netmask <= 128 then normalizeCidr addr netmask else error "Invalid IPv6 netmask"
  maybeFrom (Cidr addr netmask) = Just (addr, netmask)

-- | Direct conversion from tuple to Cidr.
instance IsMany (CidrIpAddress, Word8) Cidr where
  from (addr, netmask) = case addr of
    V4CidrIpAddress _ -> normalizeCidr addr (min netmask 32)
    V6CidrIpAddress _ _ _ _ -> normalizeCidr addr (min netmask 128)

-- | Direct conversion from Cidr to tuple.
instance IsMany Cidr (CidrIpAddress, Word8) where
  from (Cidr addr netmask) = (addr, netmask)

-- | Bidirectional conversion between tuple and Cidr.
instance Is (CidrIpAddress, Word8) Cidr

instance Is Cidr (CidrIpAddress, Word8)

-- | Normalize a CIDR address by zeroing out host bits.
-- This ensures the address represents a valid network address.
normalizeCidr :: CidrIpAddress -> Word8 -> Cidr
normalizeCidr address netmask =
  case address of
    V4CidrIpAddress addr ->
      let hostBits = 32 - min 32 (fromIntegral netmask)
          -- Create mask with network bits as 1, host bits as 0
          networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
          normalizedAddr = addr .&. networkMask
       in Cidr (V4CidrIpAddress normalizedAddr) netmask
    V6CidrIpAddress w1 w2 w3 w4 ->
      let networkBits = min 128 (fromIntegral netmask)
          normalizedWords = normalizeIPv6WordsNetwork (w1, w2, w3, w4) networkBits
       in case normalizedWords of
            (nw1, nw2, nw3, nw4) -> Cidr (V6CidrIpAddress nw1 nw2 nw3 nw4) netmask

-- | Helper function to normalize IPv6 address words by preserving only network bits.
normalizeIPv6WordsNetwork :: (Word32, Word32, Word32, Word32) -> Int -> (Word32, Word32, Word32, Word32)
normalizeIPv6WordsNetwork (w1, w2, w3, w4) networkBits
  | networkBits <= 0 = (0, 0, 0, 0) -- No network bits to preserve
  | networkBits >= 128 = (w1, w2, w3, w4) -- All bits are network bits
  | networkBits >= 96 =
      let bitsInW4 = networkBits - 96
          maskW4 = if bitsInW4 <= 0 then 0 else if bitsInW4 >= 32 then w4 else w4 .&. complement ((1 `shiftL` (32 - bitsInW4)) - 1)
       in (w1, w2, w3, maskW4)
  | networkBits >= 64 =
      let bitsInW3 = networkBits - 64
          maskW3 = if bitsInW3 <= 0 then 0 else if bitsInW3 >= 32 then w3 else w3 .&. complement ((1 `shiftL` (32 - bitsInW3)) - 1)
       in (w1, w2, maskW3, 0)
  | networkBits >= 32 =
      let bitsInW2 = networkBits - 32
          maskW2 = if bitsInW2 <= 0 then 0 else if bitsInW2 >= 32 then w2 else w2 .&. complement ((1 `shiftL` (32 - bitsInW2)) - 1)
       in (w1, maskW2, 0, 0)
  | otherwise =
      let maskW1 = if networkBits <= 0 then 0 else if networkBits >= 32 then w1 else w1 .&. complement ((1 `shiftL` (32 - networkBits)) - 1)
       in (maskW1, 0, 0, 0)
