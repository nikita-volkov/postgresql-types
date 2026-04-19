module PostgresqlTypes.Geometry
  ( Geometry (..),

    -- * Accessors
    toEWKB,

    -- * Constructors
    fromEWKB,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostGIS @geometry@ extension type. Stores the raw
-- [EWKB](https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT)
-- bytes of a geometry value; higher-level structure (point, polygon, SRID,
-- etc.) can be decoded from those bytes in application code when needed.
--
-- The @geometry@ type is not built into PostgreSQL: it is registered when
-- @CREATE EXTENSION postgis@ runs, and receives a different OID in each
-- database. Correspondingly 'baseOid' and 'arrayOid' are 'Nothing' — driver
-- adapters (e.g. the @hasql-postgresql-types@ adapter) resolve the OID by
-- 'typeName' at query time.
--
-- Both the binary and textual formats used here match PostGIS's canonical
-- serialization:
--
-- * Binary: the raw EWKB byte sequence.
-- * Textual: the upper-case hex encoding PostGIS emits from @geometry_out@
--   and accepts via @geometry_in@.
newtype Geometry = Geometry ByteString
  deriving newtype (Eq, Ord, Hashable)
  deriving (Show, Read, IsString) via (ViaIsScalar Geometry)

instance Arbitrary Geometry where
  arbitrary = Geometry . ByteString.pack <$> arbitrary
  shrink (Geometry bs) = Geometry . ByteString.pack <$> shrink (ByteString.unpack bs)

instance IsScalar Geometry where
  schemaName = Tagged Nothing
  typeName = Tagged "geometry"
  baseOid = Tagged Nothing
  arrayOid = Tagged Nothing
  typeParams = Tagged []
  binaryEncoder (Geometry bs) =
    Write.byteString bs
  binaryDecoder =
    Right . Geometry <$> PtrPeeker.remainderAsByteString
  textualEncoder (Geometry bs) =
    -- 'TextBuilder.hexadecimal' on a 'Word8' produces two hex digits.
    foldMap TextBuilder.hexadecimal (ByteString.unpack bs)
  textualDecoder = do
    hexText <- Attoparsec.takeText
    case parseHexBytes hexText of
      Left err -> fail err
      Right bytes -> pure (Geometry bytes)
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

-- | Extract the raw EWKB 'ByteString'.
toEWKB :: Geometry -> ByteString
toEWKB (Geometry bs) = bs

-- * Constructors

-- | Wrap a raw EWKB 'ByteString' as a PostGIS 'Geometry' value.
fromEWKB :: ByteString -> Geometry
fromEWKB = Geometry
