{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PostgresqlTypes.Macaddr8
  ( Macaddr8 (..),

    -- * Accessors
    toBytes,

    -- * Constructors
    fromBytes,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
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
data Macaddr8 = Macaddr8
  { macaddr8Byte1 :: Word8,
    macaddr8Byte2 :: Word8,
    macaddr8Byte3 :: Word8,
    macaddr8Byte4 :: Word8,
    macaddr8Byte5 :: Word8,
    macaddr8Byte6 :: Word8,
    macaddr8Byte7 :: Word8,
    macaddr8Byte8 :: Word8
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Macaddr8)

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

-- | Extract the 8-tuple of Word8 representing the MAC address.
toBytes :: Macaddr8 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
toBytes (Macaddr8 a b c d e f g h) = (a, b, c, d, e, f, g, h)

-- * Constructors

-- | Construct a PostgreSQL 'Macaddr8' from an 8-tuple of Word8.
fromBytes :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) -> Macaddr8
fromBytes (a, b, c, d, e, f, g, h) = Macaddr8 a b c d e f g h
