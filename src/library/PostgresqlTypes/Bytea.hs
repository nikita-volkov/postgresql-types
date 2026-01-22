module PostgresqlTypes.Bytea
  ( Bytea (..),

    -- * Accessors
    toByteString,

    -- * Constructors
    fromByteString,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as ByteString
import Data.Hashable (Hashable)
import qualified Data.Text as Text
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @bytea@ type. Binary data ("byte array").
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-binary.html).
newtype Bytea = Bytea ByteString
  deriving newtype (Eq, Ord, Hashable, Arbitrary)
  deriving (Show, Read, IsString) via (ViaIsScalar Bytea)

instance IsScalar Bytea where
  typeName = Tagged "bytea"
  baseOid = Tagged (Just 17)
  arrayOid = Tagged (Just 1001)
  typeParams = Tagged []
  binaryEncoder (Bytea bs) =
    Write.byteString bs
  binaryDecoder =
    Right . Bytea <$> PtrPeeker.remainderAsByteString
  textualEncoder (Bytea bs) =
    "\\x" <> foldMap TextBuilder.hexadecimal (ByteString.unpack bs)
  textualDecoder = do
    _ <- Attoparsec.string "\\x"
    hexText <- Attoparsec.takeText
    case parseHexBytes hexText of
      Left err -> fail err
      Right bytes -> pure (Bytea bytes)
    where
      parseHexBytes :: Text -> Either String ByteString
      parseHexBytes t = ByteString.pack <$> parseHexPairs (Text.unpack t)
      parseHexPairs :: [Char] -> Either String [Word8]
      parseHexPairs [] = Right []
      parseHexPairs [_] = Left "Odd number of hex digits"
      parseHexPairs (a : b : rest) = do
        byte <- hexPairToByte a b
        (byte :) <$> parseHexPairs rest
      hexPairToByte :: Char -> Char -> Either String Word8
      hexPairToByte a b = do
        high <- hexDigitToWord8 a
        low <- hexDigitToWord8 b
        pure (high * 16 + low)
      hexDigitToWord8 :: Char -> Either String Word8
      hexDigitToWord8 c
        | c >= '0' && c <= '9' = Right (fromIntegral (ord c - ord '0'))
        | c >= 'a' && c <= 'f' = Right (fromIntegral (ord c - ord 'a' + 10))
        | c >= 'A' && c <= 'F' = Right (fromIntegral (ord c - ord 'A' + 10))
        | otherwise = Left ("Invalid hex digit: " ++ [c])

-- * Accessors

-- | Extract the underlying 'ByteString' value.
toByteString :: Bytea -> ByteString
toByteString (Bytea bs) = bs

-- * Constructors

-- | Construct a PostgreSQL 'Bytea' from a 'ByteString' value.
fromByteString :: ByteString -> Bytea
fromByteString = Bytea
