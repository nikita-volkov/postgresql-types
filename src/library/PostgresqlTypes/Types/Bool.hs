module PostgresqlTypes.Types.Bool (Bool) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Bool
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Bool)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write

-- | PostgreSQL @boolean@ type. Logical Bool (@true@/@false@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-boolean.html).
newtype Bool = Bool Data.Bool.Bool
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsScalar Bool)

instance IsScalar Bool where
  typeName = Tagged "bool"
  baseOid = Tagged (Just 16)
  arrayOid = Tagged (Just 1000)
  typeParams = Tagged []
  binaryEncoder (Bool b) = Write.word8 (if b then 1 else 0)
  binaryDecoder =
    PtrPeeker.fixed do
      b <- PtrPeeker.unsignedInt1
      pure (Right (Bool (b /= 0)))
  textualEncoder (Bool b) = if b then "t" else "f"
  textualDecoder =
    (Bool True <$ Attoparsec.char 't')
      <|> (Bool False <$ Attoparsec.char 'f')
      <|> (Bool True <$ Attoparsec.string "true")
      <|> (Bool False <$ Attoparsec.string "false")

-- | Direct conversion from Haskell 'Data.Bool.Bool'.
-- This is always safe since both types represent the same values.
instance IsSome Data.Bool.Bool Bool where
  to (Bool b) = b
  maybeFrom = Just . Bool

-- | Direct conversion from PostgreSQL Bool to Haskell 'Data.Bool.Bool'.
-- This is always safe since both types represent the same values.
instance IsSome Bool Data.Bool.Bool where
  to b = Bool b
  maybeFrom (Bool b) = Just b

-- | Direct conversion from Haskell 'Data.Bool.Bool'.
-- This is a total conversion as it always succeeds.
instance IsMany Data.Bool.Bool Bool where
  onfrom = Bool

-- | Direct conversion from PostgreSQL Bool to Haskell 'Data.Bool.Bool'.
-- This is a total conversion as it always succeeds.
instance IsMany Bool Data.Bool.Bool where
  onfrom (Bool b) = b

-- | Bidirectional conversion between Haskell 'Data.Bool.Bool' and PostgreSQL Bool.
instance Is Data.Bool.Bool Bool

instance Is Bool Data.Bool.Bool
