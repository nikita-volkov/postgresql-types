module PrimitiveLayer.Types.Cidr (Cidr (ip, netmask)) where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Records
import Numeric (showHex)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Types.Ip (Ip (..))
import qualified PrimitiveLayer.Types.Ip as Ip
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @cidr@ type. IPv4 or IPv6 network address in combination with netmask.
--
-- Similar to @inet@ but specifically for network addresses in CIDR notation.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-net-types.html#DATATYPE-CIDR).
data Cidr = Cidr
  { -- | Network address (host bits must be zero)
    ip :: Ip,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    netmask :: Word8
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Cidr)

instance Bounded Cidr where
  minBound = Cidr minBound 0
  maxBound = Cidr maxBound 128

instance Arbitrary Cidr where
  arbitrary = do
    address <- arbitrary
    netmask <- case address of
      V4Ip _ -> QuickCheck.choose (0, 32)
      V6Ip _ _ _ _ -> QuickCheck.choose (0, 128)
    pure (constructCidr address netmask)
  shrink (Cidr address netmask) =
    [ constructCidr address' netmask'
    | address' <- shrink address,
      netmask' <- shrink netmask,
      case address' of
        V4Ip _ -> netmask' <= 32
        V6Ip _ _ _ _ -> netmask' <= 128
    ]

instance Mapping Cidr where
  typeName = Tagged "cidr"
  baseOid = Tagged 650
  arrayOid = Tagged 651
  binaryEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4Ip addr ->
        mconcat
          [ Write.word8 2, -- IPv4 address family
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 4, -- address length (4 bytes for IPv4)
            Write.bWord32 addr -- IPv4 address
          ]
      V6Ip w1 w2 w3 w4 ->
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
          pure (V4Ip addr)
        3 -> do
          -- IPv6
          when (addrLen /= 16) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          lift do
            PeekyBlinders.statically do
              V6Ip
                <$> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
                <*> PeekyBlinders.beUnsignedInt4
        _ -> do
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 3" (TextBuilder.toText (TextBuilder.decimal family))))

      pure (Cidr ip (fromIntegral netmask))

  textualEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4Ip addr ->
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
      V6Ip w1 w2 w3 w4 ->
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

-- | Convert from (Ip, Word8) to Cidr.
instance IsSome (Ip, Word8) Cidr where
  to (Cidr addr netmask) = (addr, netmask)
  maybeFrom (addr, netmask) =
    case addr of
      V4Ip _ ->
        if netmask <= 32
          then
            let normalized = constructCidr addr netmask
             in if normalized == Cidr addr netmask then Just normalized else Nothing
          else Nothing
      V6Ip _ _ _ _ ->
        if netmask <= 128
          then
            let normalized = constructCidr addr netmask
             in if normalized == Cidr addr netmask then Just normalized else Nothing
          else Nothing

-- | Direct conversion from tuple to Cidr.
instance IsMany (Ip, Word8) Cidr where
  onfrom (addr, netmask) = constructCidr addr netmask

-- | Normalize a CIDR address by zeroing out host bits.
-- This ensures the address represents a valid network address.
constructCidr :: Ip -> Word8 -> Cidr
constructCidr address netmask =
  case address of
    V4Ip w ->
      let clampedNetmask = min 32 netmask
          ip = Ip.maskedV4 (fromIntegral clampedNetmask) w
       in Cidr ip clampedNetmask
    V6Ip w1 w2 w3 w4 ->
      let clampedNetmask = min 128 netmask
          ip = Ip.maskedV6 (fromIntegral clampedNetmask) w1 w2 w3 w4
       in Cidr ip clampedNetmask
