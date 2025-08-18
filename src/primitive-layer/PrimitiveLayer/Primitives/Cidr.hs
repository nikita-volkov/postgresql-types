-- | PostgreSQL @cidr@ type.
-- Represents IPv4 or IPv6 network addresses (CIDR notation) in PostgreSQL.
module PrimitiveLayer.Primitives.Cidr (Cidr (..), IpAddress (..)) where

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
data IpAddress
  = -- | IPv4 address stored as 32-bit big-endian word
    IPv4 !Word32
  | -- | IPv6 address stored as four 32-bit big-endian words
    IPv6 !Word32 !Word32 !Word32 !Word32
  deriving stock (Eq, Ord, Generic, Show)

-- | PostgreSQL @cidr@ type representing IPv4 or IPv6 network addresses.
-- Similar to inet but specifically for network addresses in CIDR notation.
-- In binary format, PostgreSQL stores:
-- - 1 byte: address family (IPv4=2, IPv6=10)
-- - 1 byte: netmask bits
-- - 1 byte: is_cidr flag (1 for cidr, 0 for inet)
-- - 1 byte: address length in bytes
-- - N bytes: address (4 for IPv4, 16 for IPv6)
data Cidr = Cidr
  { -- | Network address
    cidrAddress :: !IpAddress,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    cidrNetmask :: !Word8
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Cidr)

instance Arbitrary IpAddress where
  arbitrary = do
    isIPv4 <- arbitrary :: Gen Bool
    if isIPv4
      then IPv4 <$> arbitrary
      else IPv6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (IPv4 addr) = IPv4 <$> shrink addr
  shrink (IPv6 w1 w2 w3 w4) =
    [IPv6 w1' w2' w3' w4' | (w1', w2', w3', w4') <- shrink (w1, w2, w3, w4)]

instance Arbitrary Cidr where
  arbitrary = do
    address <- arbitrary :: Gen IpAddress
    netmask <- case address of
      IPv4 _ -> arbitrary `suchThat` (<= 32) :: Gen Word8
      IPv6 _ _ _ _ -> arbitrary `suchThat` (<= 128) :: Gen Word8
    pure (Cidr address netmask)
  shrink (Cidr address netmask) =
    [ Cidr address' netmask'
    | address' <- shrink address,
      netmask' <- shrink netmask,
      case address' of
        IPv4 _ -> netmask' <= 32
        IPv6 _ _ _ _ -> netmask' <= 128
    ]

instance Primitive Cidr where
  typeName = Tagged "cidr"
  baseOid = Tagged 650
  arrayOid = Tagged 651
  binaryEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      IPv4 addr ->
        mconcat
          [ Write.word8 2, -- IPv4 address family
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 4, -- address length (4 bytes for IPv4)
            Write.bWord32 addr -- IPv4 address
          ]
      IPv6 w1 w2 w3 w4 ->
        mconcat
          [ Write.word8 10, -- IPv6 address family
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 16, -- address length (16 bytes for IPv6)
            Write.bWord32 w1,
            Write.bWord32 w2,
            Write.bWord32 w3,
            Write.bWord32 w4
          ]

  binaryDecoder = do
    family <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    netmask <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    _isCidr <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    addrLen <- PeekyBlinders.statically PeekyBlinders.unsignedInt1

    case (family, addrLen) of
      (2, 4) -> do
        -- IPv4
        addr <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        pure (Right (Cidr (IPv4 addr) (fromIntegral netmask)))
      (10, 16) -> do
        -- IPv6
        w1 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        w2 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        w3 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        w4 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        pure (Right (Cidr (IPv6 w1 w2 w3 w4) (fromIntegral netmask)))
      _ ->
        pure
          ( Left
              ( DecodingError
                  { location = ["cidr", "address-family"],
                    reason =
                      UnexpectedValueDecodingErrorReason
                        "IPv4 (family=2, len=4) or IPv6 (family=10, len=16)"
                        (Text.pack $ "family=" <> show family <> ", len=" <> show addrLen)
                  }
              )
          )

  textualEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      IPv4 addr ->
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
      IPv6 w1 w2 w3 w4 ->
        -- Convert 32-bit words to proper IPv6 hex representation
        let toHex w = 
              let h1 = (w `shiftR` 16) .&. 0xFFFF
                  h2 = w .&. 0xFFFF
              in showHex h1 "" ++ ":" ++ showHex h2 ""
         in TextBuilder.string (toHex w1)
              <> ":"
              <> TextBuilder.string (toHex w2)
              <> ":"
              <> TextBuilder.string (toHex w3)
              <> ":"
              <> TextBuilder.string (toHex w4)
              <> "/"
              <> TextBuilder.string (show netmask)

-- | Convert from (IpAddress, Word8) to Cidr.
instance IsSome (IpAddress, Word8) Cidr where
  to (Cidr addr netmask) = (addr, netmask)
  maybeFrom (addr, netmask) = 
    case addr of
      IPv4 _ -> if netmask <= 32 then Just (Cidr addr netmask) else Nothing
      IPv6 _ _ _ _ -> if netmask <= 128 then Just (Cidr addr netmask) else Nothing

-- | Convert from Cidr to (IpAddress, Word8).
instance IsSome Cidr (IpAddress, Word8) where
  to (addr, netmask) = case addr of
    IPv4 _ -> if netmask <= 32 then Cidr addr netmask else error "Invalid IPv4 netmask"
    IPv6 _ _ _ _ -> if netmask <= 128 then Cidr addr netmask else error "Invalid IPv6 netmask"
  maybeFrom (Cidr addr netmask) = Just (addr, netmask)

-- | Direct conversion from tuple to Cidr.
instance IsMany (IpAddress, Word8) Cidr where
  from (addr, netmask) = case addr of
    IPv4 _ -> Cidr addr (min netmask 32)
    IPv6 _ _ _ _ -> Cidr addr (min netmask 128)

-- | Direct conversion from Cidr to tuple.
instance IsMany Cidr (IpAddress, Word8) where
  from (Cidr addr netmask) = (addr, netmask)

-- | Bidirectional conversion between tuple and Cidr.
instance Is (IpAddress, Word8) Cidr

instance Is Cidr (IpAddress, Word8)
