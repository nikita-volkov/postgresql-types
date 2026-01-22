module PostgresqlTypes.Macaddr8
  ( Macaddr8 (..),

    -- * Accessors
    toByte1,
    toByte2,
    toByte3,
    toByte4,
    toByte5,
    toByte6,
    toByte7,
    toByte8,

    -- * Constructors
    fromBytes,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Hashable (Hashable (..))
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @macaddr8@ type. 8-byte MAC (Media Access Control) address in EUI-64 format.
--
-- The format is eight groups of two hexadecimal digits, separated by colons.
-- Example: @01:23:45:67:89:ab:cd:ef@
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-net-types.html#DATATYPE-MACADDR8).
data Macaddr8
  = Macaddr8
      -- | First byte
      Word8
      -- | Second byte
      Word8
      -- | Third byte
      Word8
      -- | Fourth byte
      Word8
      -- | Fifth byte
      Word8
      -- | Sixth byte
      Word8
      -- | Seventh byte
      Word8
      -- | Eighth byte
      Word8
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Macaddr8)

instance Arbitrary Macaddr8 where
  arbitrary = do
    bytes <- replicateM 8 arbitrary
    -- Ensure not all bytes are zero as that might be invalid
    if all (== 0) bytes
      then pure (Macaddr8 0 0 0 0 0 0 0 1) -- Use a valid non-zero MAC
      else case bytes of
        [a, b, c, d, e, f, g, h] -> pure (Macaddr8 a b c d e f g h)
        _ -> error "impossible case"
  shrink (Macaddr8 a b c d e f g h) =
    [ Macaddr8 a' b' c' d' e' f' g' h'
    | (a', b', c', d', e', f', g', h') <- shrink (a, b, c, d, e, f, g, h),
      not (a' == 0 && b' == 0 && c' == 0 && d' == 0 && e' == 0 && f' == 0 && g' == 0 && h' == 0)
    ]

instance Hashable Macaddr8 where
  hashWithSalt salt (Macaddr8 a b c d e f g h) =
    salt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
      `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h

instance IsScalar Macaddr8 where
  typeName = Tagged "macaddr8"
  baseOid = Tagged (Just 774)
  arrayOid = Tagged (Just 775)
  typeParams = Tagged []
  binaryEncoder (Macaddr8 a b c d e f g h) =
    mconcat
      [ Write.word8 a,
        Write.word8 b,
        Write.word8 c,
        Write.word8 d,
        Write.word8 e,
        Write.word8 f,
        Write.word8 g,
        Write.word8 h
      ]
  binaryDecoder =
    PtrPeeker.fixed
      ( Right
          <$> ( Macaddr8
                  <$> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
              )
      )
  textualEncoder (Macaddr8 a b c d e f g h) =
    TextBuilder.intercalate ":" $
      [ formatByte a,
        formatByte b,
        formatByte c,
        formatByte d,
        formatByte e,
        formatByte f,
        formatByte g,
        formatByte h
      ]
    where
      formatByte :: Word8 -> TextBuilder.TextBuilder
      formatByte x = TextBuilder.string (printf "%02x" x)
  textualDecoder = do
    a <- hexByte
    _ <- Attoparsec.char ':'
    b <- hexByte
    _ <- Attoparsec.char ':'
    c <- hexByte
    _ <- Attoparsec.char ':'
    d <- hexByte
    _ <- Attoparsec.char ':'
    e <- hexByte
    _ <- Attoparsec.char ':'
    f <- hexByte
    _ <- Attoparsec.char ':'
    g <- hexByte
    _ <- Attoparsec.char ':'
    h <- hexByte
    pure (Macaddr8 a b c d e f g h)
    where
      hexByte = do
        h1 <- hexDigit
        h2 <- hexDigit
        pure (h1 * 16 + h2)
      hexDigit =
        (\c -> fromIntegral (ord c - ord '0'))
          <$> Attoparsec.satisfy (\c -> c >= '0' && c <= '9')
          <|> (\c -> fromIntegral (ord c - ord 'a' + 10))
            <$> Attoparsec.satisfy (\c -> c >= 'a' && c <= 'f')
          <|> (\c -> fromIntegral (ord c - ord 'A' + 10))
            <$> Attoparsec.satisfy (\c -> c >= 'A' && c <= 'F')

-- * Accessors

-- | Extract the first byte of the MAC address.
toByte1 :: Macaddr8 -> Word8
toByte1 (Macaddr8 a _ _ _ _ _ _ _) = a

-- | Extract the second byte of the MAC address.
toByte2 :: Macaddr8 -> Word8
toByte2 (Macaddr8 _ b _ _ _ _ _ _) = b

-- | Extract the third byte of the MAC address.
toByte3 :: Macaddr8 -> Word8
toByte3 (Macaddr8 _ _ c _ _ _ _ _) = c

-- | Extract the fourth byte of the MAC address.
toByte4 :: Macaddr8 -> Word8
toByte4 (Macaddr8 _ _ _ d _ _ _ _) = d

-- | Extract the fifth byte of the MAC address.
toByte5 :: Macaddr8 -> Word8
toByte5 (Macaddr8 _ _ _ _ e _ _ _) = e

-- | Extract the sixth byte of the MAC address.
toByte6 :: Macaddr8 -> Word8
toByte6 (Macaddr8 _ _ _ _ _ f _ _) = f

-- | Extract the seventh byte of the MAC address.
toByte7 :: Macaddr8 -> Word8
toByte7 (Macaddr8 _ _ _ _ _ _ g _) = g

-- | Extract the eighth byte of the MAC address.
toByte8 :: Macaddr8 -> Word8
toByte8 (Macaddr8 _ _ _ _ _ _ _ h) = h

-- * Constructors

-- | Construct a PostgreSQL 'Macaddr8' from 8 bytes.
fromBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Macaddr8
fromBytes = Macaddr8
