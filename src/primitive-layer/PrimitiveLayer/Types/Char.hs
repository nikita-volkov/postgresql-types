module PrimitiveLayer.Types.Char (Char) where

import qualified Data.ByteString as ByteString
import qualified Data.Char
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Char)
import PrimitiveLayer.Via
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
  deriving (Show) via (ViaPrimitive Char)

instance Arbitrary Char where
  arbitrary =
    Char <$> QuickCheck.choose (0, 127)

instance Mapping Char where
  typeName = Tagged "char"
  baseOid = Tagged 18
  arrayOid = Tagged 1002
  binaryEncoder (Char base) =
    Write.word8 base
  binaryDecoder =
    Right . Char <$> PeekyBlinders.statically PeekyBlinders.unsignedInt1
  textualEncoder (Char base) =
    TextBuilder.unicodeCodepoint (fromIntegral base)

instance IsSome Word8 Char where
  to = coerce
  maybeFrom word8 =
    if word8 > 127
      then Nothing
      else Just (Char word8)

instance IsMany Word8 Char where
  from word8 = Char (clearBit word8 7)

instance IsSome Data.Char.Char Char where
  to (Char word8) = Data.Char.chr (fromIntegral word8)
  maybeFrom char =
    let ord = Data.Char.ord char
     in if ord > 127
          then Nothing
          else Just (Char (fromIntegral ord))

-- | Turns invalid chars into '\NUL'.
instance IsMany Data.Char.Char Char where
  from = fromMaybe (Char 0) . maybeFrom
