module PostgresqlTypes.Char
  ( Char,

    -- * Accessors
    toWord8,
    toChar,

    -- * Constructors
    refineFromWord8,
    normalizeFromWord8,
    refineFromChar,
    normalizeFromChar,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Char
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Char)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @\"char\"@ type (note the quotes). Single-byte internal PostgreSQL type.
--
-- This is a special PostgreSQL type that uses only 1 byte of storage and can store
-- a single ASCII character (values 0-127). It is primarily used in PostgreSQL system
-- catalogs as a simplistic enumeration type and is not intended for general-purpose use.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-character.html).
--
-- __Important distinction:__ This type represents the quoted @\"char\"@ type in PostgreSQL,
-- which is completely different from @char(n)@, @character(n)@, or @bpchar(n)@:
--
-- * @\"char\"@ (this type) — single-byte internal type, 1 byte storage, used in system catalogs
-- * @char(n)@, @character(n)@, @bpchar(n)@ — fixed-length blank-padded strings, represented by 'PostgresqlTypes.Bpchar.Bpchar'
--
-- For example, @\"char\"@ in SQL is 'Char' in Haskell, while @char(1)@ in SQL is
-- @'PostgresqlTypes.Bpchar.Bpchar' 1@ in Haskell. Despite the similar names,
-- these are entirely different types in PostgreSQL.
newtype Char = Char Word8
  deriving newtype (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Char)

instance Arbitrary Char where
  arbitrary =
    Char <$> QuickCheck.choose (0, 127)

instance IsScalar Char where
  typeName = Tagged "char"
  baseOid = Tagged (Just 18)
  arrayOid = Tagged (Just 1002)
  typeSignature = Tagged "\"char\""
  binaryEncoder (Char base) =
    Write.word8 base
  binaryDecoder =
    Right . Char <$> PtrPeeker.fixed PtrPeeker.unsignedInt1
  textualEncoder (Char base) =
    TextBuilder.unicodeCodepoint (fromIntegral base)
  textualDecoder = do
    -- PostgreSQL may return empty string for \NUL or stripped spaces
    maybeC <- Attoparsec.option Nothing (Just <$> Attoparsec.anyChar)
    case maybeC of
      Nothing -> pure (Char 0) -- Empty input means \NUL
      Just c -> do
        let charOrd = ord c
        if charOrd > 127
          then fail "Invalid char: value > 127"
          else pure (Char (fromIntegral charOrd))

-- * Accessors

-- | Extract the underlying 'Word8' value.
toWord8 :: Char -> Word8
toWord8 = coerce

-- | Convert PostgreSQL 'Char' to Haskell 'Data.Char.Char'.
toChar :: Char -> Data.Char.Char
toChar (Char word8) = Data.Char.chr (fromIntegral word8)

-- * Constructors

-- | Construct a PostgreSQL 'Char' from 'Word8' with validation.
-- Returns 'Nothing' if the value is greater than 127.
refineFromWord8 :: Word8 -> Maybe Char
refineFromWord8 word8 =
  if word8 > 127
    then Nothing
    else Just (Char word8)

-- | Construct a PostgreSQL 'Char' from 'Word8', clamping to valid range.
-- Values greater than 127 have the high bit cleared.
normalizeFromWord8 :: Word8 -> Char
normalizeFromWord8 word8 = Char (clearBit word8 7)

-- | Construct a PostgreSQL 'Char' from Haskell 'Data.Char.Char' with validation.
-- Returns 'Nothing' if the character's code point is greater than 127.
refineFromChar :: Data.Char.Char -> Maybe Char
refineFromChar char =
  let ord = Data.Char.ord char
   in if ord > 127
        then Nothing
        else Just (Char (fromIntegral ord))

-- | Construct a PostgreSQL 'Char' from Haskell 'Data.Char.Char'.
-- Turns invalid chars into '\NUL'.
normalizeFromChar :: Data.Char.Char -> Char
normalizeFromChar = fromMaybe (Char 0) . refineFromChar
