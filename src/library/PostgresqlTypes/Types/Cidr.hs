{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Cidr (Cidr) where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Bits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Types.Ip (Ip (..))
import qualified PostgresqlTypes.Types.Ip as Ip
import PostgresqlTypes.Via
import qualified PtrPeeker
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
  deriving (Show) via (ViaIsStandardType Cidr)

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

instance IsStandardType Cidr where
  typeName = Tagged "cidr"
  baseOid = Tagged (Just 650)
  arrayOid = Tagged (Just 651)
  typeParams = Tagged []
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
      PtrPeeker.fixed do
        (,,,)
          <$> PtrPeeker.unsignedInt1
          <*> PtrPeeker.unsignedInt1
          <*> PtrPeeker.unsignedInt1
          <*> PtrPeeker.unsignedInt1

    runExceptT do
      when (isCidrFlag /= 1) do
        throwError (DecodingError ["is-cidr"] (UnexpectedValueDecodingErrorReason "1" (TextBuilder.toText (TextBuilder.decimal isCidrFlag))))

      ip <- case family of
        2 -> do
          -- IPv4
          when (addrLen /= 4) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "4" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          addr <- lift do
            PtrPeeker.fixed PtrPeeker.beUnsignedInt4
          pure (V4Ip addr)
        3 -> do
          -- IPv6
          when (addrLen /= 16) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          lift do
            PtrPeeker.fixed do
              V6Ip
                <$> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
        _ -> do
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 3" (TextBuilder.toText (TextBuilder.decimal family))))

      pure (Cidr ip (fromIntegral netmask))

  textualEncoder (Cidr ipAddr netmask) =
    case ipAddr of
      V4Ip addr ->
        let a = ((addr `shiftR` 24) .&. 0xFF)
            b = ((addr `shiftR` 16) .&. 0xFF)
            c = ((addr `shiftR` 8) .&. 0xFF)
            d = (addr .&. 0xFF)
         in mconcat
              [ TextBuilder.decimal a,
                ".",
                TextBuilder.decimal b,
                ".",
                TextBuilder.decimal c,
                ".",
                TextBuilder.decimal d,
                "/",
                TextBuilder.decimal netmask
              ]
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
  textualDecoder = do
    ipAddr <- parseIp
    _ <- Attoparsec.char '/'
    netmask <- Attoparsec.decimal
    pure (constructCidr ipAddr netmask)
    where
      parseIp = parseV6 <|> parseV4
      parseV4 = do
        a <- Attoparsec.decimal @Word32
        _ <- Attoparsec.char '.'
        b <- Attoparsec.decimal @Word32
        _ <- Attoparsec.char '.'
        c <- Attoparsec.decimal @Word32
        _ <- Attoparsec.char '.'
        d <- Attoparsec.decimal @Word32
        pure (V4Ip ((a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d))
      parseV6 = do
        -- Try to parse compressed IPv6 (with ::)
        parseCompressedV6 <|> parseFullV6

      parseFullV6 = do
        groups <- parseHexGroup `Attoparsec.sepBy1` Attoparsec.char ':'
        when (length groups /= 8) (fail "Expected 8 groups")
        case groups of
          [h1, h2, h3, h4, h5, h6, h7, h8] ->
            pure
              ( V6Ip
                  (fromIntegral h1 `shiftL` 16 .|. fromIntegral h2)
                  (fromIntegral h3 `shiftL` 16 .|. fromIntegral h4)
                  (fromIntegral h5 `shiftL` 16 .|. fromIntegral h6)
                  (fromIntegral h7 `shiftL` 16 .|. fromIntegral h8)
              )
          _ -> fail "Expected 8 groups"

      parseCompressedV6 = do
        -- Check if starts with ::
        startsWithDoubleColon <- Attoparsec.option False (True <$ Attoparsec.string "::")
        before <-
          if startsWithDoubleColon
            then pure []
            else parseHexGroup `Attoparsec.sepBy1` Attoparsec.char ':'
        -- Check for :: in the middle (if we didn't already find it at start)
        hasDoubleColon <-
          if startsWithDoubleColon
            then pure True
            else Attoparsec.option False (True <$ Attoparsec.string "::")
        after <-
          if hasDoubleColon
            then parseHexGroup `Attoparsec.sepBy` Attoparsec.char ':'
            else pure []
        -- If no :: was found, this isn't compressed format
        when (not hasDoubleColon) (fail "Not a compressed IPv6 address")
        -- Expand to 8 groups, filling middle with zeros
        let totalGroups = length before + length after
        when (totalGroups > 7) (fail "Too many groups in compressed IPv6")
        let zeros = replicate (8 - totalGroups) 0
            allGroups = before ++ zeros ++ after
        case allGroups of
          [h1, h2, h3, h4, h5, h6, h7, h8] ->
            pure
              ( V6Ip
                  (fromIntegral h1 `shiftL` 16 .|. fromIntegral h2)
                  (fromIntegral h3 `shiftL` 16 .|. fromIntegral h4)
                  (fromIntegral h5 `shiftL` 16 .|. fromIntegral h6)
                  (fromIntegral h7 `shiftL` 16 .|. fromIntegral h8)
              )
          _ -> fail "Expected 8 groups after expansion"

      parseHexGroup = Attoparsec.hexadecimal @Word16

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
