-- |
-- This module provides a bridge between PostgreSQL's standard types and the postgresql-simple library,
-- offering automatic ToField and FromField instance generation for types that implement the 'IsStandardType' constraint.
--
-- = Usage
--
-- Import this module in addition to @Database.PostgreSQL.Simple@ to get encoding/decoding support
-- for postgresql-types in postgresql-simple queries:
--
-- > import Database.PostgreSQL.Simple
-- > import PostgresqlSimple.PostgresqlTypes
-- > import PostgresqlTypes.Types (Int4, Text)
-- >
-- > -- Now you can use postgresql-types directly in queries
-- > example :: Connection -> Int4 -> IO [Only Text]
-- > example conn myInt = query conn "SELECT name FROM users WHERE id = ?" (Only myInt)
--
-- = How it works
--
-- * 'toFieldVia' creates a 'ToField' compatible 'Action' using the 'textualEncoder' from 'IsStandardType'
-- * 'fromFieldVia' creates a 'FromField' compatible parser using the 'textualDecoder' from 'IsStandardType'
--
-- The module uses textual format for encoding/decoding since that's what postgresql-simple primarily uses.
module PostgresqlSimple.PostgresqlTypes
  ( IsStandardType,
    toFieldVia,
    fromFieldVia,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple.FromField (Conversion, FieldParser, ResultError (..), returnError)
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (Action (..))
import PostgresqlSimple.PostgresqlTypes.Prelude
import PostgresqlTypes
import qualified TextBuilder

-- | Convert a postgresql-types value to a postgresql-simple 'Action'.
--
-- This function uses the textual encoder from 'IsStandardType' to produce
-- an escaped text value suitable for use in SQL queries.
--
-- > instance ToField Int4 where
-- >   toField = toFieldVia
toFieldVia :: forall a. (IsStandardType a) => a -> Action
toFieldVia value =
  Escape (Text.encodeUtf8 (to (textualEncoder value)))

-- | Parse a postgresql-types value from a postgresql-simple field.
--
-- This function uses the textual decoder from 'IsStandardType' to parse
-- values received from PostgreSQL in text format.
--
-- > instance FromField Int4 where
-- >   fromField = fromFieldVia
fromFieldVia :: forall a. (Typeable a, IsStandardType a) => FieldParser a
fromFieldVia field mdata = case mdata of
  Nothing -> returnError UnexpectedNull field ""
  Just bytes -> case Text.decodeUtf8' bytes of
    Left err ->
      returnError ConversionFailed field $
        "UTF-8 decoding failed: " <> show err
    Right text ->
      case Attoparsec.parseOnly (textualDecoder @a <* Attoparsec.endOfInput) text of
        Left err ->
          returnError ConversionFailed field $
            "Parsing failed: " <> err
        Right value -> pure value
