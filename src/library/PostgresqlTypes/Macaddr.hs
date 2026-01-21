module PostgresqlTypes.Macaddr
  ( Macaddr,

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

-- | PostgreSQL @macaddr@ type. MAC (Media Access Control) address.
--
-- Represents a @6@-byte MAC address, typically used in networking.
-- The format is six groups of two hexadecimal digits, separated by colons.
-- Example: @01:23:45:67:89:ab@
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-net-types.html#DATATYPE-MACADDR).
data Macaddr
  = Macaddr
      Word8
      Word8
      Word8
      Word8
      Word8
      Word8
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar Macaddr)

instance Arbitrary Macaddr where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    pure (Macaddr a b c d e f)
  shrink (Macaddr a b c d e f) =
    [ Macaddr a' b' c' d' e' f'
    | a' <- shrink a,
      b' <- shrink b,
      c' <- shrink c,
      d' <- shrink d,
      e' <- shrink e,
      f' <- shrink f
    ]

instance IsScalar Macaddr where
  typeName = Tagged "macaddr"
  baseOid = Tagged (Just 829)
  arrayOid = Tagged (Just 1040)
  typeParams = Tagged []
  binaryEncoder (Macaddr a b c d e f) =
    mconcat
      [ Write.word8 a,
        Write.word8 b,
        Write.word8 c,
        Write.word8 d,
        Write.word8 e,
        Write.word8 f
      ]
  binaryDecoder =
    PtrPeeker.fixed
      ( Right
          <$> ( Macaddr
                  <$> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
              )
      )
  textualEncoder (Macaddr a b c d e f) =
    (TextBuilder.intercalate ":")
      [ TextBuilder.hexadecimal a,
        TextBuilder.hexadecimal b,
        TextBuilder.hexadecimal c,
        TextBuilder.hexadecimal d,
        TextBuilder.hexadecimal e,
        TextBuilder.hexadecimal f
      ]
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
    pure (Macaddr a b c d e f)
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

-- | Extract the 6-tuple of Word8 representing the MAC address.
toBytes :: Macaddr -> (Word8, Word8, Word8, Word8, Word8, Word8)
toBytes (Macaddr a b c d e f) = (a, b, c, d, e, f)

-- * Constructors

-- | Construct a PostgreSQL 'Macaddr' from a 6-tuple of Word8.
fromBytes :: (Word8, Word8, Word8, Word8, Word8, Word8) -> Macaddr
fromBytes (a, b, c, d, e, f) = Macaddr a b c d e f
