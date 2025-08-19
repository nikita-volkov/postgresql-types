-- | PostgreSQL @cidr@ type.
-- Represents IPv4 or IPv6 network addresses (CIDR notation) in PostgreSQL.
module PrimitiveLayer.Primitives.Cidr (Cidr (ip, netmask), CidrIp (..)) where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Records
import Numeric (showHex)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import Test.QuickCheck (Gen, suchThat)
import qualified TextBuilder

-- | IP address type for representing IPv4 and IPv6 addresses.
data CidrIp
  = -- | IPv4 address stored as 32-bit big-endian word
    V4CidrIp Word32
  | -- | IPv6 address stored as four 32-bit big-endian words
    V6CidrIp Word32 Word32 Word32 Word32
  deriving stock (Eq, Ord, Show)

instance Bounded CidrIp where
  minBound = V4CidrIp 0
  maxBound = V6CidrIp maxBound maxBound maxBound maxBound

-- | PostgreSQL @cidr@ type representing IPv4 or IPv6 network addresses.
-- Similar to @inet@ but specifically for network addresses in CIDR notation.
data Cidr = Cidr
  { -- | Network address (host bits must be zero)
    ip :: CidrIp,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    netmask :: Word8
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Cidr)

instance Bounded Cidr where
  minBound = Cidr minBound 0
  maxBound = Cidr maxBound 128

instance Arbitrary CidrIp where
  arbitrary = do
    isIPv4 <- arbitrary :: Gen Bool
    if isIPv4
      then V4CidrIp <$> arbitrary
      else V6CidrIp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (V4CidrIp addr) = V4CidrIp <$> shrink addr
  shrink (V6CidrIp w1 w2 w3 w4) =
    [V6CidrIp w1' w2' w3' w4' | (w1', w2', w3', w4') <- shrink (w1, w2, w3, w4)]

instance Arbitrary Cidr where
  arbitrary = do
    address <- arbitrary :: Gen CidrIp
    netmask <- case address of
      V4CidrIp _ -> arbitrary `suchThat` (<= 32) :: Gen Word8
      V6CidrIp _ _ _ _ -> arbitrary `suchThat` (<= 128) :: Gen Word8
    pure (constructCidr address netmask)
  shrink (Cidr address netmask) =
    [ constructCidr address' netmask'
    | address' <- shrink address,
      netmask' <- shrink netmask,
      case address' of
        V4CidrIp _ -> netmask' <= 32
        V6CidrIp _ _ _ _ -> netmask' <= 128
    ]

instance Primitive Cidr where
  typeName = Tagged "cidr"
  baseOid = Tagged 650
  arrayOid = Tagged 651
  binaryEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4CidrIp addr ->
        mconcat
          [ Write.word8 2, -- IPv4 address family
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 4, -- address length (4 bytes for IPv4)
            Write.bWord32 addr -- IPv4 address
          ]
      V6CidrIp w1 w2 w3 w4 ->
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
          pure (V4CidrIp addr)
        3 -> do
          -- IPv6
          when (addrLen /= 16) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          lift do
            PeekyBlinders.statically do
              V6CidrIp
                <$> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
        _ -> do
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 3" (TextBuilder.toText (TextBuilder.decimal family))))

      pure (Cidr ip (fromIntegral netmask))

  textualEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4CidrIp addr ->
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
      V6CidrIp w1 w2 w3 w4 ->
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

-- | Convert from (CidrIp, Word8) to Cidr.
instance IsSome (CidrIp, Word8) Cidr where
  to (Cidr addr netmask) = (addr, netmask)
  maybeFrom (addr, netmask) =
    case addr of
      V4CidrIp _ -> if netmask <= 32 then Just (constructCidr addr netmask) else Nothing
      V6CidrIp _ _ _ _ -> if netmask <= 128 then Just (constructCidr addr netmask) else Nothing

-- | Direct conversion from tuple to Cidr.
instance IsMany (CidrIp, Word8) Cidr where
  from (addr, netmask) = constructCidr addr netmask

-- | Normalize a CIDR address by zeroing out host bits.
-- This ensures the address represents a valid network address.
constructCidr :: CidrIp -> Word8 -> Cidr
constructCidr address netmask =
  case address of
    V4CidrIp addr ->
      let clampedNetmask = min 32 (fromIntegral netmask)
          hostBits = 32 - clampedNetmask
          -- Create mask with network bits as 1, host bits as 0
          networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
          normalizedAddr = addr .&. networkMask
       in Cidr (V4CidrIp normalizedAddr) (fromIntegral clampedNetmask)
    V6CidrIp w1 w2 w3 w4 ->
      let clampedNetmask = min 128 (fromIntegral netmask)
          ipAddress =
            if
              | clampedNetmask <= 0 -> V6CidrIp 0 0 0 0 -- No network bits to preserve
              | clampedNetmask >= 128 -> V6CidrIp w1 w2 w3 w4 -- All bits are network bits
              | clampedNetmask >= 96 -> V6CidrIp w1 w2 w3 (mask (clampedNetmask - 96) w4)
              | clampedNetmask >= 64 -> V6CidrIp w1 w2 (mask (clampedNetmask - 64) w3) 0
              | clampedNetmask >= 32 -> V6CidrIp w1 (mask (clampedNetmask - 32) w2) 0 0
              | otherwise -> V6CidrIp (mask clampedNetmask w1) 0 0 0
       in Cidr ipAddress (fromIntegral clampedNetmask)
      where
        mask a b =
          if a <= 0
            then 0
            else
              if a >= 32
                then b
                else b .&. complement ((1 `shiftL` (32 - a)) - 1)
