module PostgresqlTypes.Cidr
  ( Cidr,

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
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (fold)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @cidr@ type.
--
-- Holds an IPv4 or IPv6 network specification (network address + netmask) where host bits must be zero.
-- If you want to store individual host addresses with optional netmasks, use the @inet@ type instead.
--
-- The input format for this type is @address/y@ where @address@ is an IPv4 or IPv6 network address (with host bits zeroed) and @y@ is the number of bits in the netmask. The @/y@ portion is required for @cidr@ type.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-net-types.html#DATATYPE-CIDR).
data Cidr
  = V4Cidr
      -- | IPv4 network address stored as 32-bit big-endian word (host bits must be zero).
      Word32
      -- | Network mask length (0-32).
      Word8
  | V6Cidr
      -- | First 32 bits of IPv6 network address in big-endian order.
      Word32
      -- | Second 32 bits of IPv6 network address in big-endian order.
      Word32
      -- | Third 32 bits of IPv6 network address in big-endian order.
      Word32
      -- | Fourth 32 bits of IPv6 network address in big-endian order.
      Word32
      -- | Network mask length (0-128).
      Word8
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Cidr)

instance Arbitrary Cidr where
  arbitrary = do
    isIPv4 <- arbitrary
    if isIPv4
      then do
        addr <- arbitrary
        netmask <- QuickCheck.choose (0, 32)
        pure (normalizeFromV4 addr netmask)
      else do
        w1 <- arbitrary
        w2 <- arbitrary
        w3 <- arbitrary
        w4 <- arbitrary
        netmask <- QuickCheck.choose (0, 128)
        pure (normalizeFromV6 w1 w2 w3 w4 netmask)

  shrink = \case
    V4Cidr addr netmask ->
      [ normalizeFromV4 addr' netmask'
      | addr' <- shrink addr,
        netmask' <- shrink netmask,
        netmask' <= 32
      ]
    V6Cidr w1 w2 w3 w4 netmask ->
      [ normalizeFromV6 w1' w2' w3' w4' netmask'
      | (w1', w2', w3', w4') <- shrink (w1, w2, w3, w4),
        netmask' <- shrink netmask,
        netmask' <= 128
      ]

instance IsScalar Cidr where
  schemaName = Tagged Nothing
  typeName = Tagged "cidr"
  baseOid = Tagged (Just 650)
  arrayOid = Tagged (Just 651)
  typeParams = Tagged []

  binaryEncoder = \case
    V4Cidr addr netmask ->
      mconcat
        [ Write.word8 2, -- IPv4 address family
          Write.word8 netmask,
          Write.word8 1, -- is_cidr flag (1 for cidr)
          Write.word8 4, -- address length (4 bytes for IPv4)
          Write.bWord32 addr -- IPv4 network address
        ]
    V6Cidr w1 w2 w3 w4 netmask ->
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

      case family of
        2 -> do
          -- IPv4
          when (addrLen /= 4) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "4" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          addr <- lift do
            PtrPeeker.fixed PtrPeeker.beUnsignedInt4
          pure (V4Cidr addr (fromIntegral netmask))
        3 -> do
          -- IPv6
          when (addrLen /= 16) do
            throwError (DecodingError ["address-length"] (UnexpectedValueDecodingErrorReason "16" (TextBuilder.toText (TextBuilder.decimal addrLen))))
          lift do
            PtrPeeker.fixed do
              V6Cidr
                <$> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> PtrPeeker.beUnsignedInt4
                <*> pure (fromIntegral netmask)
        _ -> do
          throwError (DecodingError ["address-family"] (UnexpectedValueDecodingErrorReason "2 or 3" (TextBuilder.toText (TextBuilder.decimal family))))

  textualEncoder = \case
    V4Cidr addr netmask ->
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
    V6Cidr w1 w2 w3 w4 netmask ->
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
        _ <- Attoparsec.char '/'
        netmask <- Attoparsec.decimal :: Attoparsec.Parser Word8
        let addr = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
        pure (normalizeFromV4 addr netmask)

      parseV6 = do
        -- Try to parse compressed IPv6 (with ::)
        parseCompressedV6 <|> parseFullV6

      parseFullV6 = do
        groups <- parseHexGroup `Attoparsec.sepBy1` Attoparsec.char ':'
        when (length groups /= 8) (fail "Expected 8 groups")
        _ <- Attoparsec.char '/'
        netmask <- Attoparsec.decimal :: Attoparsec.Parser Word8
        case groups of
          [h1, h2, h3, h4, h5, h6, h7, h8] -> do
            let w1 = fromIntegral h1 `shiftL` 16 .|. fromIntegral h2
                w2 = fromIntegral h3 `shiftL` 16 .|. fromIntegral h4
                w3 = fromIntegral h5 `shiftL` 16 .|. fromIntegral h6
                w4 = fromIntegral h7 `shiftL` 16 .|. fromIntegral h8
            pure (normalizeFromV6 w1 w2 w3 w4 netmask)
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
        _ <- Attoparsec.char '/'
        netmask <- Attoparsec.decimal :: Attoparsec.Parser Word8
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
            pure (normalizeFromV6 w1 w2 w3 w4 netmask)
          _ -> fail "Expected 8 groups after expansion"

      parseHexGroup = Attoparsec.hexadecimal @Word16

-- * Accessors

-- |
-- Pattern match on 'Cidr' type.
fold ::
  -- | Function to handle IPv4 network case.
  --
  -- Takes the IPv4 network address as 'Word32' and the netmask as 'Word8' both in big-endian order.
  (Word32 -> Word8 -> a) ->
  -- | Function to handle IPv6 network case.
  --
  -- Takes the four 32-bit words of the IPv6 network address in big-endian order and the netmask as 'Word8'.
  (Word32 -> Word32 -> Word32 -> Word32 -> Word8 -> a) ->
  (Cidr -> a)
fold fV4 fV6 = \case
  V4Cidr addr netmask -> fV4 addr netmask
  V6Cidr w1 w2 w3 w4 netmask -> fV6 w1 w2 w3 w4 netmask

-- | Refine an IPv4 'Cidr' value from a 32-bit network address and an 8-bit netmask, both in big-endian order.
refineToV4 :: Cidr -> Maybe (Word32, Word8)
refineToV4 = \case
  V4Cidr addr netmask -> Just (addr, netmask)
  V6Cidr {} -> Nothing

-- | Refine an IPv6 'Cidr' value from four 32-bit words representing the network address and an 8-bit netmask, all in big-endian order.
refineToV6 :: Cidr -> Maybe (Word32, Word32, Word32, Word32, Word8)
refineToV6 = \case
  V4Cidr {} -> Nothing
  V6Cidr w1 w2 w3 w4 netmask -> Just (w1, w2, w3, w4, netmask)

-- * Constructors

-- |
-- Construct an IPv4 'Cidr' value from a 32-bit address and an 8-bit netmask, both in big-endian order.
--
-- The netmask is clamped to be in the range 0-32.
-- Host bits are automatically zeroed to ensure a valid network address.
normalizeFromV4 ::
  -- | IPv4 address as a 32-bit big-endian word.
  Word32 ->
  -- | Network mask length (0-32).
  Word8 ->
  Cidr
normalizeFromV4 addr netmask =
  let clampedNetmask = min 32 netmask
      hostBits = 32 - fromIntegral clampedNetmask
      networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
      networkAddr = addr .&. networkMask
   in V4Cidr networkAddr clampedNetmask

-- |
-- Construct an IPv6 'Cidr' value from four 32-bit words representing the address and an 8-bit netmask, all in big-endian order.
--
-- The netmask is clamped to be in the range 0-128.
-- Host bits are automatically zeroed to ensure a valid network address.
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
  Cidr
normalizeFromV6 w1 w2 w3 w4 netmask =
  let clampedNetmask = min 128 netmask
      nm = fromIntegral clampedNetmask
      (nw1, nw2, nw3, nw4) = applyV6Mask nm w1 w2 w3 w4
   in V6Cidr nw1 nw2 nw3 nw4 clampedNetmask

-- | Refine an IPv4 'Cidr' value from a 32-bit address and an 8-bit netmask, both in big-endian order.
--
-- Returns 'Nothing' if the netmask is out of range (greater than 32) or if host bits are not zero.
refineFromV4 ::
  -- | IPv4 address as a 32-bit big-endian word.
  Word32 ->
  -- | Network mask length (0-32).
  Word8 ->
  Maybe Cidr
refineFromV4 addr netmask
  | netmask > 32 = Nothing
  | otherwise =
      let hostBits = 32 - fromIntegral netmask
          networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
          networkAddr = addr .&. networkMask
       in if networkAddr == addr
            then Just (V4Cidr addr netmask)
            else Nothing

-- | Refine an IPv6 'Cidr' value from four 32-bit words representing the address and an 8-bit netmask, all in big-endian order.
--
-- Returns 'Nothing' if the netmask is out of range (greater than 128) or if host bits are not zero.
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
  Maybe Cidr
refineFromV6 w1 w2 w3 w4 netmask
  | netmask > 128 = Nothing
  | otherwise =
      let (nw1, nw2, nw3, nw4) = applyV6Mask (fromIntegral netmask) w1 w2 w3 w4
       in if (nw1, nw2, nw3, nw4) == (w1, w2, w3, w4)
            then Just (V6Cidr w1 w2 w3 w4 netmask)
            else Nothing

-- | Helper function to apply IPv6 network mask by zeroing out host bits.
--
-- Takes a netmask length (0-128) and four 32-bit words representing the IPv6 address.
-- Returns the four 32-bit words with host bits zeroed according to the netmask.
--
-- The masking is applied word by word, starting from the most significant word.
-- Each word represents 32 bits, so:
-- - netmask 0-32 affects only w1
-- - netmask 33-64 affects w1 and w2
-- - netmask 65-96 affects w1, w2, and w3
-- - netmask 97-128 affects all four words
applyV6Mask :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> (Word32, Word32, Word32, Word32)
applyV6Mask netmask w1 w2 w3 w4
  | netmask <= 0 = (0, 0, 0, 0)
  | netmask >= 128 = (w1, w2, w3, w4)
  | netmask >= 96 = (w1, w2, w3, maskWord (netmask - 96) w4)
  | netmask >= 64 = (w1, w2, maskWord (netmask - 64) w3, 0)
  | netmask >= 32 = (w1, maskWord (netmask - 32) w2, 0, 0)
  | otherwise = (maskWord netmask w1, 0, 0, 0)
  where
    maskWord bits word
      | bits <= 0 = 0
      | bits >= 32 = word
      | otherwise =
          let hostBits = 32 - bits
              networkMask = complement ((1 `shiftL` hostBits) - 1)
           in word .&. networkMask
