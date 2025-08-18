-- | PostgreSQL @inet@ type.
-- Represents an IPv4 or IPv6 host address, optionally with subnet mask.
module PrimitiveLayer.Primitives.Inet (Inet (..)) where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.Socket as Network
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import Test.QuickCheck (Gen)
import qualified TextBuilder

-- | PostgreSQL @inet@ type wrapper around 'Network.SockAddr'.
--
-- The inet type can store IPv4 or IPv6 addresses, with optional subnet mask.
-- In binary format, PostgreSQL stores:
-- - 1 byte: address family (IPv4=2, IPv6=10)
-- - 1 byte: netmask bits
-- - 1 byte: is_cidr flag (0 for inet)
-- - 1 byte: address length in bytes
-- - N bytes: address (4 for IPv4, 16 for IPv6)
newtype Inet = Inet Network.SockAddr
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Inet)

instance Arbitrary Inet where
  arbitrary = do
    -- Generate IPv4 addresses for simplicity in tests
    a <- arbitrary :: Gen Word8
    b <- arbitrary :: Gen Word8
    c <- arbitrary :: Gen Word8
    d <- arbitrary :: Gen Word8
    let addr = fromIntegral a * 16777216 + fromIntegral b * 65536 + fromIntegral c * 256 + fromIntegral d
    pure (Inet (Network.SockAddrInet 0 addr))
  shrink (Inet (Network.SockAddrInet _ addr)) =
    [Inet (Network.SockAddrInet 0 addr') | addr' <- shrink addr]
  shrink _ = []

instance Primitive Inet where
  typeName = Tagged "inet"
  baseOid = Tagged 869
  arrayOid = Tagged 1041

  binaryEncoder (Inet sockAddr) = case sockAddr of
    Network.SockAddrInet _ hostAddr ->
      -- IPv4: family(2) + netmask(32) + is_cidr(0) + len(4) + 4 bytes
      mconcat
        [ Write.word8 2, -- AF_INET
          Write.word8 32, -- netmask bits (32 for host address)
          Write.word8 0, -- is_cidr flag (0 for inet)
          Write.word8 4, -- address length
          Write.bWord32 hostAddr
        ]
    Network.SockAddrInet6 _ _ hostAddr6 _ ->
      -- IPv6: family(10) + netmask(128) + is_cidr(0) + len(16) + 16 bytes
      let (w1, w2, w3, w4) = hostAddr6
       in mconcat
            [ Write.word8 10, -- AF_INET6
              Write.word8 128, -- netmask bits (128 for host address)
              Write.word8 0, -- is_cidr flag (0 for inet)
              Write.word8 16, -- address length
              Write.bWord32 w1,
              Write.bWord32 w2,
              Write.bWord32 w3,
              Write.bWord32 w4
            ]
    _ ->
      -- Fallback for other address types - treat as IPv4 0.0.0.0
      mconcat
        [ Write.word8 2, -- AF_INET
          Write.word8 32, -- netmask bits
          Write.word8 0, -- is_cidr flag
          Write.word8 4, -- address length
          Write.bWord32 0 -- 0.0.0.0
        ]

  binaryDecoder = do
    family <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    netmask <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    isCidr <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
    addrLen <- PeekyBlinders.statically PeekyBlinders.unsignedInt1

    case family of
      2 -> do
        -- IPv4
        if addrLen /= 4
          then
            pure
              $ Left
              $ DecodingError
                { location = ["inet", "ipv4-address-length"],
                  reason = UnexpectedValueDecodingErrorReason "4" (TextBuilder.toText (TextBuilder.decimal addrLen))
                }
          else do
            hostAddr <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            pure $ Right $ Inet $ Network.SockAddrInet 0 hostAddr
      10 -> do
        -- IPv6
        if addrLen /= 16
          then
            pure
              $ Left
              $ DecodingError
                { location = ["inet", "ipv6-address-length"],
                  reason = UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))
                }
          else do
            w1 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            w2 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            w3 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            w4 <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            pure $ Right $ Inet $ Network.SockAddrInet6 0 0 (w1, w2, w3, w4) 0
      _ ->
        pure
          $ Left
          $ DecodingError
            { location = ["inet", "address-family"],
              reason = UnexpectedValueDecodingErrorReason "2 or 10" (TextBuilder.toText (TextBuilder.decimal family))
            }

  textualEncoder (Inet sockAddr) = case sockAddr of
    Network.SockAddrInet _ hostAddr ->
      -- Format IPv4 as dotted decimal
      let a = fromIntegral $ (hostAddr `shiftR` 24) .&. 0xFF
          b = fromIntegral $ (hostAddr `shiftR` 16) .&. 0xFF
          c = fromIntegral $ (hostAddr `shiftR` 8) .&. 0xFF
          d = fromIntegral $ hostAddr .&. 0xFF
       in TextBuilder.decimal a
            <> "."
            <> TextBuilder.decimal b
            <> "."
            <> TextBuilder.decimal c
            <> "."
            <> TextBuilder.decimal d
    Network.SockAddrInet6 _ _ hostAddr6 _ ->
      -- Format IPv6 in standard notation (simplified)
      let (w1, w2, w3, w4) = hostAddr6
       in TextBuilder.text (fromString (show w1))
            <> ":"
            <> TextBuilder.text (fromString (show w2))
            <> ":"
            <> TextBuilder.text (fromString (show w3))
            <> ":"
            <> TextBuilder.text (fromString (show w4))
    _ ->
      -- Fallback for unknown address types
      "0.0.0.0"

-- | Direct conversion from 'Network.SockAddr'.
-- Note: Only SockAddrInet and SockAddrInet6 are supported.
instance IsSome Network.SockAddr Inet where
  to (Inet addr) = addr
  maybeFrom addr = case addr of
    Network.SockAddrInet {} -> Just (Inet addr)
    Network.SockAddrInet6 {} -> Just (Inet addr)
    _ -> Nothing -- Other address types not supported

-- | Direct conversion from PostgreSQL Inet to 'Network.SockAddr'.
instance IsSome Inet Network.SockAddr where
  to addr = Inet addr
  maybeFrom (Inet addr) = Just addr

-- | Partial conversion from 'Network.SockAddr' that filters unsupported types.
instance IsMany Network.SockAddr Inet where
  from addr = case addr of
    Network.SockAddrInet {} -> Inet addr
    Network.SockAddrInet6 {} -> Inet addr
    _ -> Inet (Network.SockAddrInet 0 0) -- Fallback to 0.0.0.0

-- | Direct conversion from PostgreSQL Inet to 'Network.SockAddr'.
instance IsMany Inet Network.SockAddr where
  from (Inet addr) = addr

-- | Bidirectional conversion between supported 'Network.SockAddr' types and Inet.
instance Is Network.SockAddr Inet

instance Is Inet Network.SockAddr
