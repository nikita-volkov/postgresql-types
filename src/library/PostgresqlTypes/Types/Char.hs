module PostgresqlTypes.Types.Char (Char) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Char
import qualified Data.Text as Text
import qualified GHC.TypeLits as TypeLits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Char)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @char(n)@ type. Fixed-length character string.
--
-- The type parameter @numChars@ specifies the static length of the character string.
-- For the single-byte @char@ type (not @char(n)@), use @Char 1@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html).
newtype Char (numChars :: TypeLits.Nat) = Char Word8
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Char numChars))

instance (TypeLits.KnownNat numChars) => Arbitrary (Char numChars) where
  arbitrary =
    Char <$> QuickCheck.choose (0, 127)

instance (TypeLits.KnownNat numChars) => IsStandardType (Char numChars) where
  typeName = Tagged "char"
  baseOid = Tagged (Just 18)
  arrayOid = Tagged (Just 1002)
  typeParams =
    Tagged
      ( let len = TypeLits.natVal (Proxy @numChars)
         in if len == 1
              then [] -- char without length modifier
              else [Text.pack (show len)] -- char(n)
      )
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

instance (TypeLits.KnownNat numChars) => IsSome Word8 (Char numChars) where
  to = coerce
  maybeFrom word8 =
    if word8 > 127
      then Nothing
      else Just (Char word8)

instance (TypeLits.KnownNat numChars) => IsMany Word8 (Char numChars) where
  onfrom word8 = Char (clearBit word8 7)

instance (TypeLits.KnownNat numChars) => IsSome Data.Char.Char (Char numChars) where
  to (Char word8) = Data.Char.chr (fromIntegral word8)
  maybeFrom char =
    let ord = Data.Char.ord char
     in if ord > 127
          then Nothing
          else Just (Char (fromIntegral ord))

-- | Turns invalid chars into '\NUL'.
instance (TypeLits.KnownNat numChars) => IsMany Data.Char.Char (Char numChars) where
  onfrom = fromMaybe (Char 0) . maybeFrom
