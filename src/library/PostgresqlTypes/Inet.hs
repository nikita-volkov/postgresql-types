module PostgresqlTypes.Inet
  ( Inet,

    -- * Accessors
    fold,
    refineToV4,
    refineToV6,

    -- * Constructors
    normalizeFromV4,
    normalizeFromV6,
    refineFromV4,
    refineFromV6,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Bits
import Data.Hashable (Hashable (..))
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (fold)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @inet@ type.
--
-- Holds an IPv4 or IPv6 host address, and optionally its subnet, all in one field. The subnet is represented by the number of network address bits present in the host address (the “netmask”). If the netmask is 32 and the address is IPv4, then the value does not indicate a subnet, only a single host. In IPv6, the address length is 128 bits, so 128 bits specify a unique host address. Note that if you want to accept only networks, you should use the @cidr@ type rather than @inet@.
--
-- The input format for this type is @address/y@ where @address@ is an IPv4 or IPv6 address and @y@ is the number of bits in the netmask. If the @/y@ portion is omitted, the netmask is taken to be 32 for IPv4 or 128 for IPv6, so the value represents just a single host. On display, the @/y@ portion is suppressed if the netmask specifies a single host.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-net-types.html#DATATYPE-INET).
data Inet
  = V4Inet
      -- | IPv4 address stored as 32-bit big-endian word.
      Word32
      -- | Network mask length (0-32).
      Word8
  | V6Inet
      -- | First 32 bits of IPv6 address in big-endian order.
      Word32
      -- | Second 32 bits of IPv6 address in big-endian order.
      Word32
      -- | Third 32 bits of IPv6 address in big-endian order.
      Word32
      -- | Fourth 32 bits of IPv6 address in big-endian order.
      Word32
      -- | Network mask length (0-128).
      Word8
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Inet)

instance Arbitrary Inet where
  arbitrary = do
    isIPv4 <- arbitrary
    if isIPv4
      then
        V4Inet
          <$> arbitrary
          <*> QuickCheck.choose (0, 32)
      else
        V6Inet
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> QuickCheck.choose (0, 128)

  shrink = \case
    V4Inet addr netmask ->
      [ V4Inet addr' netmask'
      | addr' <- shrink addr,
        netmask' <- shrink netmask,
        netmask' <= 32
      ]
    V6Inet w1 w2 w3 w4 netmask ->
      [ V6Inet w1' w2' w3' w4' netmask'
      | (w1', w2', w3', w4') <- shrink (w1, w2, w3, w4),
        netmask' <- shrink netmask,
        netmask' <= 128
      ]

instance Hashable Inet where
  hashWithSalt salt = \case
    V4Inet addr netmask -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` addr `hashWithSalt` netmask
    V6Inet w1 w2 w3 w4 netmask -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` w1 `hashWithSalt` w2 `hashWithSalt` w3 `hashWithSalt` w4 `hashWithSalt` netmask

instance IsScalar Inet where
  typeName = Tagged "inet"
  baseOid = Tagged (Just 869)
  arrayOid = Tagged (Just 1041)
  typeParams = Tagged []

  binaryEncoder = \case
    V4Inet addr netmask ->
      mconcat
        [ Write.word8 2, -- IPv4 address family
          Write.word8 netmask,
          Write.word8 0, -- is_cidr flag (0 for inet)
          Write.word8 4, -- address length (4 bytes for IPv4)
          Write.bWord32 addr -- IPv4 address
        ]
    V6Inet w1 w2 w3 w4 netmask ->
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

      case family of
        2 -> do
          -- IPv4
          when (addrLen /= 4) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "4" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          addr <- lift do
            PtrPeeker.fixed PtrPeeker.beUnsignedInt4
          pure (V4Inet addr (fromIntegral netmask))
        3 -> do
          -- IPv6
          when (addrLen /= 16) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          lift do
            PtrPeeker.fixed do
              V6Inet
                <$> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> pure (fromIntegral netmask)
        _ -> do
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 10" (TextBuilder.toText (TextBuilder.decimal family))))

  textualEncoder = \case
    V4Inet addr netmask ->
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
    V6Inet w1 w2 w3 w4 netmask ->
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
  textualDecoder = parseV6 <|> parseV4
    where
      parseV4 = do
        a <- Attoparsec.decimal @Word32
        _ <- Attoparsec.char '.'
        b <- Attoparsec.decimal @Word32
        _ <- Attoparsec.char '.'
        c <- Attoparsec.decimal @Word32
        _ <- Attoparsec.char '.'
        d <- Attoparsec.decimal @Word32
        let addr = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
        netmask <- optional (Attoparsec.char '/' *> (Attoparsec.decimal :: Attoparsec.Parser Word8))
        pure (V4Inet addr (fromMaybe 32 netmask))
      parseV6 = do
        -- Try to parse compressed IPv6 (with ::)
        parseCompressedV6 <|> parseFullV6

      parseFullV6 = do
        groups <- parseHexGroup `Attoparsec.sepBy1` Attoparsec.char ':'
        when (length groups /= 8) (fail "Expected 8 groups")
        case groups of
          [h1, h2, h3, h4, h5, h6, h7, h8] -> do
            let w1 = fromIntegral h1 `shiftL` 16 .|. fromIntegral h2
                w2 = fromIntegral h3 `shiftL` 16 .|. fromIntegral h4
                w3 = fromIntegral h5 `shiftL` 16 .|. fromIntegral h6
                w4 = fromIntegral h7 `shiftL` 16 .|. fromIntegral h8
            netmask <- optional (Attoparsec.char '/' *> (Attoparsec.decimal :: Attoparsec.Parser Word8))
            pure (V6Inet w1 w2 w3 w4 (fromMaybe 128 netmask))
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
          [h1, h2, h3, h4, h5, h6, h7, h8] -> do
            let w1 = fromIntegral h1 `shiftL` 16 .|. fromIntegral h2
                w2 = fromIntegral h3 `shiftL` 16 .|. fromIntegral h4
                w3 = fromIntegral h5 `shiftL` 16 .|. fromIntegral h6
                w4 = fromIntegral h7 `shiftL` 16 .|. fromIntegral h8
            netmask <- optional (Attoparsec.char '/' *> (Attoparsec.decimal :: Attoparsec.Parser Word8))
            pure (V6Inet w1 w2 w3 w4 (fromMaybe 128 netmask))
          _ -> fail "Expected 8 groups after expansion"

      parseHexGroup = Attoparsec.hexadecimal @Word16

-- * Accessors

-- |
-- Pattern match on 'Inet' type.
fold ::
  -- | Function to handle IPv4 address case.
  --
  -- Takes the IPv4 address as 'Word32' and the netmask as 'Word8' both in big-endian order.
  (Word32 -> Word8 -> a) ->
  -- | Function to handle IPv6 address case.
  --
  -- Takes the four 32-bit words of the IPv6 address in big-endian order and the netmask as 'Word8'.
  (Word32 -> Word32 -> Word32 -> Word32 -> Word8 -> a) ->
  (Inet -> a)
fold fV4 fV6 = \case
  V4Inet addr netmask -> fV4 addr netmask
  V6Inet w1 w2 w3 w4 netmask -> fV6 w1 w2 w3 w4 netmask

-- | Refine an IPv4 'Inet' value from a 32-bit address and an 8-bit netmask, both in big-endian order.
refineToV4 :: Inet -> Maybe (Word32, Word8)
refineToV4 = \case
  V4Inet addr netmask -> Just (addr, netmask)
  V6Inet {} -> Nothing

-- | Refine an IPv6 'Inet' value from four 32-bit words representing the address and an 8-bit netmask, all in big-endian order.
refineToV6 :: Inet -> Maybe (Word32, Word32, Word32, Word32, Word8)
refineToV6 = \case
  V4Inet {} -> Nothing
  V6Inet w1 w2 w3 w4 netmask -> Just (w1, w2, w3, w4, netmask)

-- * Constructors

-- |
-- Construct an IPv4 'Inet' value from a 32-bit address and an 8-bit netmask, both in big-endian order.
--
-- The netmask is clamped to be in the range 0-32.
normalizeFromV4 ::
  -- | IPv4 address as a 32-bit big-endian word.
  Word32 ->
  -- | Network mask length (0-32).
  Word8 ->
  Inet
normalizeFromV4 addr netmask =
  V4Inet addr (min 32 netmask)

-- |
-- Construct an IPv6 'Inet' value from four 32-bit words representing the address and an 8-bit netmask, all in big-endian order.
--
-- The netmask is clamped to be in the range 0-128.
normalizeFromV6 ::
  -- | First 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Second 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Third 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Fourth 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Network mask length (0-128).
  Word8 ->
  Inet
normalizeFromV6 w1 w2 w3 w4 netmask =
  V6Inet w1 w2 w3 w4 (min 128 netmask)

-- | Refine an IPv4 'Inet' value from a 32-bit address and an 8-bit netmask, both in big-endian order.
--
-- Returns 'Nothing' if the netmask is out of range (greater than 32).
refineFromV4 ::
  -- | IPv4 address as a 32-bit big-endian word.
  Word32 ->
  -- | Network mask length (0-32).
  Word8 ->
  Maybe Inet
refineFromV4 addr netmask
  | netmask <= 32 = Just (V4Inet addr netmask)
  | otherwise = Nothing

-- | Refine an IPv6 'Inet' value from four 32-bit words representing the address and an 8-bit netmask, all in big-endian order.
--
-- Returns 'Nothing' if the netmask is out of range (greater than 128).
refineFromV6 ::
  -- | First 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Second 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Third 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Fourth 32 bits of IPv6 address in big-endian order.
  Word32 ->
  -- | Network mask length (0-128).
  Word8 ->
  Maybe Inet
refineFromV6 w1 w2 w3 w4 netmask
  | netmask <= 128 = Just (V6Inet w1 w2 w3 w4 netmask)
  | otherwise = Nothing
