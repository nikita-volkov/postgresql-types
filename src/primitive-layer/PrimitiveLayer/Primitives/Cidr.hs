-- | PostgreSQL @cidr@ type.
-- Represents IPv4 or IPv6 network addresses (CIDR notation) in PostgreSQL.
module PrimitiveLayer.Primitives.Cidr (Cidr (..)) where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.Socket as Network
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import Test.QuickCheck (Gen, suchThat)
import qualified TextBuilder

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
    cidrAddress :: !Network.SockAddr,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    cidrNetmask :: !Word8
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Cidr)

instance Arbitrary Cidr where
  arbitrary = do
    -- Generate IPv4 networks for simplicity in tests
    a <- arbitrary :: Gen Word8
    b <- arbitrary :: Gen Word8
    c <- arbitrary :: Gen Word8
    d <- arbitrary :: Gen Word8
    netmask <- arbitrary `suchThat` (<= 32) :: Gen Word8
    let addr = fromIntegral a * 16777216 + fromIntegral b * 65536 + fromIntegral c * 256 + fromIntegral d
    pure (Cidr (Network.SockAddrInet 0 addr) netmask)
  shrink (Cidr (Network.SockAddrInet _ addr) netmask) =
    [ Cidr (Network.SockAddrInet 0 addr') netmask'
    | addr' <- shrink addr,
      netmask' <- shrink netmask,
      netmask' <= 32
    ]
  shrink (Cidr addr netmask) =
    [Cidr addr netmask' | netmask' <- shrink netmask]

instance Primitive Cidr where
  typeName = Tagged "cidr"
  baseOid = Tagged 650
  arrayOid = Tagged 651
  binaryEncoder (Cidr sockAddr netmask) =
    case sockAddr of
      Network.SockAddrInet _ addr ->
        mconcat
          [ Write.word8 2, -- IPv4 address family
            Write.word8 netmask,
            Write.word8 1, -- is_cidr flag (1 for cidr)
            Write.word8 4, -- address length (4 bytes for IPv4)
            Write.bWord32 addr -- IPv4 address
          ]
      Network.SockAddrInet6 _ _ (w1, w2, w3, w4) _ ->
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
      _ -> mempty -- Other address types not supported

  binaryDecoder = do
    family <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    netmask <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    _isCidr <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    addrLen <- PeekyBlinders.statically PeekyBlinders.unsignedInt1

    case (family, addrLen) of
      (2, 4) -> do
        -- IPv4
        addr <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        pure (Right (Cidr (Network.SockAddrInet 0 addr) (fromIntegral netmask)))
      (10, 16) -> do
        -- IPv6
        w1 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        w2 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        w3 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        w4 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
        pure (Right (Cidr (Network.SockAddrInet6 0 0 (w1, w2, w3, w4) 0) (fromIntegral netmask)))
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

  textualEncoder (Cidr sockAddr netmask) =
    case sockAddr of
      Network.SockAddrInet _ addr ->
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
      Network.SockAddrInet6 _ _ (w1, w2, w3, w4) _ ->
        -- Simplified IPv6 representation
        TextBuilder.string (show w1)
          <> ":"
          <> TextBuilder.string (show w2)
          <> ":"
          <> TextBuilder.string (show w3)
          <> ":"
          <> TextBuilder.string (show w4)
          <> "::/"
          <> TextBuilder.string (show netmask)
      _ -> "unknown/" <> TextBuilder.string (show netmask)

-- | Convert from (Network.SockAddr, Word8) to Cidr.
instance IsSome (Network.SockAddr, Word8) Cidr where
  to (Cidr addr netmask) = (addr, netmask)
  maybeFrom (addr, netmask) = Just (Cidr addr netmask)

-- | Convert from Cidr to (Network.SockAddr, Word8).
instance IsSome Cidr (Network.SockAddr, Word8) where
  to (addr, netmask) = Cidr addr netmask
  maybeFrom (Cidr addr netmask) = Just (addr, netmask)

-- | Direct conversion from tuple to Cidr.
instance IsMany (Network.SockAddr, Word8) Cidr where
  from (addr, netmask) = Cidr addr netmask

-- | Direct conversion from Cidr to tuple.
instance IsMany Cidr (Network.SockAddr, Word8) where
  from (Cidr addr netmask) = (addr, netmask)

-- | Bidirectional conversion between tuple and Cidr.
instance Is (Network.SockAddr, Word8) Cidr

instance Is Cidr (Network.SockAddr, Word8)
