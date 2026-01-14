module Demo.Statements.SelectArtistById where

import Demo.Types
import PostgresqlTypes.Mapping
import Prelude hiding (Int8, Text)

data SelectArtistById = SelectArtistById
  { id :: Int8
  }

data SelectArtistByIdResultRow = SelectArtistByIdResultRow
  { name :: Text,
    genres :: Vector Genre
  }

instance ParameterizesStatement SelectArtistById where
  type ResultOf SelectArtistById = Maybe SelectArtistByIdResultRow
  statementOf =
    StatementOf
      { sql = "SELECT name, genres FROM artist WHERE id = $1",
        params =
          mconcat
            [ (.id) >$< param d0 nonNullable
            ],
        result =
          someRow
            ( SelectArtistByIdResultRow
                <$> column d0 nonNullable
                <*> column (d1 nonNullable vector) nonNullable
            )
      }
