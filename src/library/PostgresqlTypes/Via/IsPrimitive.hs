module PostgresqlTypes.Via.IsPrimitive where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude

newtype ViaIsPrimitive a = ViaIsPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, IsPrimitive)

instance (IsPrimitive a) => Show (ViaIsPrimitive a) where
  showsPrec d (ViaIsPrimitive a) = showsPrec d (textualEncoder a)

instance (IsPrimitive a) => Read (ViaIsPrimitive a) where
  readsPrec d str =
    [ (ViaIsPrimitive a, rest)
    | (txt, rest) <- readsPrec d str,
      let parsed = Attoparsec.parseOnly (textualDecoder @a <* Attoparsec.endOfInput) txt,
      Right a <- [parsed]
    ]

instance (IsPrimitive a) => IsString (ViaIsPrimitive a) where
  fromString string =
    case Attoparsec.parseOnly (textualDecoder @a <* Attoparsec.endOfInput) (Text.pack string) of
      Left err -> error ("ViaIsPrimitive fromString: failed to parse: " <> err)
      Right a -> ViaIsPrimitive a
