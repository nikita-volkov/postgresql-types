module PostgresqlTypes.Types.Char (Char) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Char
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Char)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @char@ type. Fixed-length character string.
-- @7@-bit value, occupying @1@ byte in the DB typically used for storing an ASCII character.
--
-- Not to confuse with @character(n)@ or @char(n)@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html).
newtype Char = Char Word8
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Char)

instance Arbitrary Char where
  arbitrary =
    Char <$> QuickCheck.choose (0, 127)

instance IsStandardType Char where
  typeName = Tagged "char"
  baseOid = Tagged (Just 18)
  arrayOid = Tagged (Just 1002)
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

instance IsSome Word8 Char where
  to = coerce
  maybeFrom word8 =
    if word8 > 127
      then Nothing
      else Just (Char word8)

instance IsMany Word8 Char where
  onfrom word8 = Char (clearBit word8 7)

instance IsSome Data.Char.Char Char where
  to (Char word8) = Data.Char.chr (fromIntegral word8)
  maybeFrom char =
    let ord = Data.Char.ord char
     in if ord > 127
          then Nothing
          else Just (Char (fromIntegral ord))

-- | Turns invalid chars into '\NUL'.
instance IsMany Data.Char.Char Char where
  onfrom = fromMaybe (Char 0) . maybeFrom
