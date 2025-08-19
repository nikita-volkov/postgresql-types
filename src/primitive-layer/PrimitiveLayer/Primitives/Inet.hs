-- | PostgreSQL @inet@ type.
-- Represents an IPv4 or IPv6 host address, optionally with subnet mask.
module PrimitiveLayer.Primitives.Inet (Inet (ip, netmask)) where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Primitives.Ip (Ip (..))
import qualified PrimitiveLayer.Primitives.Ip as Ip
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @inet@ type representing IPv4 or IPv6 host addresses.
-- Similar to @cidr@ but specifically for host addresses with optional subnet masks.
data Inet = Inet
  { -- | Host address
    ip :: Ip,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    netmask :: Word8
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Inet)

instance Bounded Inet where
  minBound = Inet minBound 0
  maxBound = Inet maxBound 128

instance Arbitrary Inet where
  arbitrary = do
    address <- arbitrary
    netmask <- case address of
      V4Ip _ -> QuickCheck.choose (0, 32)
      V6Ip _ _ _ _ -> QuickCheck.choose (0, 128)
    pure (Inet address netmask)
  shrink (Inet address netmask) =
    [ Inet address' netmask'
    | address' <- shrink address,
      netmask' <- shrink netmask,
      case address' of
        V4Ip _ -> netmask' <= 32
        V6Ip _ _ _ _ -> netmask' <= 128
    ]

instance Primitive Inet where
  typeName = Tagged "inet"
  baseOid = Tagged 869
  arrayOid = Tagged 1041

  binaryEncoder (Inet ipAddr netmask) =
    case ipAddr of
      V4Ip addr ->
        mconcat
          [ Write.word8 2, -- IPv4 address family
            Write.word8 netmask,
            Write.word8 0, -- is_cidr flag (0 for inet)
            Write.word8 4, -- address length (4 bytes for IPv4)
            Write.bWord32 addr -- IPv4 address
          ]
      V6Ip w1 w2 w3 w4 ->
        mconcat
          [ Write.word8 3, -- IPv6 address family for INET (different from CIDR)
            Write.word8 netmask,
            Write.word8 0, -- is_cidr flag (0 for inet)
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
      when (isCidrFlag /= 0) do
        throwError (DecodingError ["is-cidr"] (UnexpectedValueDecodingErrorReason "0" (TextBuilder.toText (TextBuilder.decimal isCidrFlag))))

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
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 10" (TextBuilder.toText (TextBuilder.decimal family))))

      pure (Inet ip (fromIntegral netmask))

  textualEncoder (Inet ipAddr netmask) =
    case ipAddr of
      V4Ip addr ->
        let a = fromIntegral ((addr `shiftR` 24) .&. 0xFF)
            b = fromIntegral ((addr `shiftR` 16) .&. 0xFF)
            c = fromIntegral ((addr `shiftR` 8) .&. 0xFF)
            d = fromIntegral (addr .&. 0xFF)
         in if netmask == 32
              then -- Host address without explicit netmask
                TextBuilder.string (show a)
                  <> "."
                  <> TextBuilder.string (show b)
                  <> "."
                  <> TextBuilder.string (show c)
                  <> "."
                  <> TextBuilder.string (show d)
              else -- Host address with netmask
                TextBuilder.string (show a)
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
            ipStr =
              toHex w1
                <> ":"
                <> toHex w2
                <> ":"
                <> toHex w3
                <> ":"
                <> toHex w4
         in if netmask == 128
              then ipStr -- Host address without explicit netmask
              else ipStr <> "/" <> TextBuilder.decimal netmask -- Host address with netmask

instance IsSome (Ip, Word8) Inet where
  to (Inet addr netmask) = (addr, netmask)

instance IsSome Inet (Ip, Word8) where
  to (addr, netmask) = Inet addr netmask

instance IsMany (Ip, Word8) Inet

instance IsMany Inet (Ip, Word8)

instance Is (Ip, Word8) Inet

instance Is Inet (Ip, Word8)
