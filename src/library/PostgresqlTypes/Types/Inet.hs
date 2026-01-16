{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Types.Inet (Inet) where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Bits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Types.Ip (Ip (..))
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @inet@ type. IPv4 or IPv6 host address network address in combination with netmask.
--
-- Similar to @cidr@ but specifically for host addresses with optional subnet masks.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-net-types.html#DATATYPE-INET).
data Inet = Inet
  { -- | Host address
    ip :: Ip,
    -- | Network mask length (0-32 for IPv4, 0-128 for IPv6)
    netmask :: Word8
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Inet)

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

instance IsStandardType Inet where
  typeName = Tagged "inet"
  baseOid = Tagged (Just 869)
  arrayOid = Tagged (Just 1041)
  runtimeTypeParams _ = []

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
      PtrPeeker.fixed do
        (,,,)
          <$> PtrPeeker.unsignedInt1
          <*> PtrPeeker.unsignedInt1
          <*> PtrPeeker.unsignedInt1
          <*> PtrPeeker.unsignedInt1

    runExceptT do
      when (isCidrFlag /= 0) do
        throwError (DecodingError ["is-cidr"] (UnexpectedValueDecodingErrorReason "0" (TextBuilder.toText (TextBuilder.decimal isCidrFlag))))

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
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 10" (TextBuilder.toText (TextBuilder.decimal family))))

      pure (Inet ip (fromIntegral netmask))

  textualEncoder (Inet ipAddr netmask) =
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
                if netmask == 32
                  then mempty
                  else "/" <> TextBuilder.decimal netmask
              ]
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
  textualDecoder = do
    ipAddr <- parseIp
    netmask <- optional (Attoparsec.char '/' *> (Attoparsec.decimal :: Attoparsec.Parser Word8))
    let defaultNetmask = case ipAddr of
          V4Ip _ -> 32
          V6Ip {} -> 128
    pure (Inet ipAddr (fromMaybe defaultNetmask netmask))
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
        -- Check for :: in the middle or if we already found it at start
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

instance IsSome (Ip, Word8) Inet where
  to (Inet addr netmask) = (addr, netmask)

instance IsSome Inet (Ip, Word8) where
  to (addr, netmask) = Inet addr netmask

instance IsMany (Ip, Word8) Inet

instance IsMany Inet (Ip, Word8)

instance Is (Ip, Word8) Inet

instance Is Inet (Ip, Word8)
