module PostgresqlTypes.Via.IsScalar where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude

newtype ViaIsScalar a = ViaIsScalar a
  deriving newtype (Eq, Ord, Arbitrary, IsScalar)

instance (IsScalar a) => Show (ViaIsScalar a) where
  showsPrec d (ViaIsScalar a) = showsPrec d (textualEncoder a)

instance (IsScalar a) => Read (ViaIsScalar a) where
  readsPrec d str =
    [ (ViaIsScalar a, rest)
    | (txt, rest) <- readsPrec d str,
      let parsed = Attoparsec.parseOnly (textualDecoder @a <* Attoparsec.endOfInput) txt,
      Right a <- [parsed]
    ]

instance (IsScalar a) => IsString (ViaIsScalar a) where
  fromString string =
    case Attoparsec.parseOnly (textualDecoder @a <* Attoparsec.endOfInput) (Text.pack string) of
      Left err -> error ("ViaIsScalar fromString: failed to parse: " <> err)
      Right a -> ViaIsScalar a
