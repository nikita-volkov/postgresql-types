module PostgresqlTypes.Bool
  ( Bool,

    -- * Accessors
    toBool,

    -- * Constructors
    fromBool,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Bool
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Bool)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write

-- | PostgreSQL @boolean@ type. Logical Bool (@true@/@false@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-boolean.html).
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

-- * Accessors

-- | Extract the underlying Haskell 'Data.Bool.Bool' value.
toBool :: Bool -> Data.Bool.Bool
toBool (Bool b) = b

-- * Constructors

-- | Construct a PostgreSQL 'Bool' from a Haskell 'Data.Bool.Bool' value.
fromBool :: Data.Bool.Bool -> Bool
fromBool = Bool
