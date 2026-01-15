module PostgresqlTypes.Algebra where

import qualified Data.Attoparsec.Text as Attoparsec
import PostgresqlTypes.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Evidence that a type maps to a PostgreSQL value.
class IsStandardType a where
  -- | PostgreSQL type identifiers.
  typeIdsOf :: TypeIdsOf a

  -- | Encode the value in PostgreSQL binary format.
  binaryEncoder :: a -> Write.Write

  -- | Decode the value from PostgreSQL binary format.
  binaryDecoder :: PtrPeeker.Variable (Either DecodingError a)

  -- | Represent the value in PostgreSQL textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder

  -- | Decode the value from PostgreSQL textual format.
  textualDecoder :: Attoparsec.Parser a

-- | Evidence that a type can be used as an element of a PostgreSQL range type.
class (IsStandardType a, Ord a) => IsRangeElement a where
  rangeTypeIdsOf :: TypeIdsOf a

-- | Evidence that a type can be used as an element of a PostgreSQL multirange type.
class (IsRangeElement a) => IsMultirangeElement a where
  multirangeTypeIdsOf :: TypeIdsOf a

data TypeIdsOf a = TypeIdsOf
  { name :: Text,
    stableBaseOid :: Maybe Word32,
    stableArrayOid :: Maybe Word32
  }
  deriving stock (Show, Eq)

data DecodingError = DecodingError
  { location :: [Text],
    reason :: DecodingErrorReason
  }
  deriving stock (Show, Eq)

data DecodingErrorReason
  = ParsingDecodingErrorReason
      -- | Details.
      Text
      -- | Input.
      ByteString
  | UnexpectedValueDecodingErrorReason
      -- | Expected.
      Text
      -- | Actual.
      Text
  | -- | Unsupported server-side value.
    UnsupportedValueDecodingErrorReason
      -- | Details.
      Text
      -- | Value.
      Text
  deriving stock (Show, Eq)
