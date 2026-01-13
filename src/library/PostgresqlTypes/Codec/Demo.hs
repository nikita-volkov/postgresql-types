module PostgresqlTypes.Codec.Demo where

import PostgresqlTypes.Codec.Algebra
import PostgresqlTypes.Codec.Prelude hiding (Int8, Text)
import PostgresqlTypes.Primitive.Types

data Genre = Rock | Pop | Jazz

instance IsEnum Genre where
  enumSchema = Tagged ""
  enumName = Tagged "genre"
  enumVariants =
    [ ("rock", Rock),
      ("pop", Pop),
      ("jazz", Jazz)
    ]

data Artist = Artist
  { name :: Text,
    genre :: Genre
  }

instance IsComposite Artist where
  compositeSchema = Tagged ""
  compositeName = Tagged "artist"
  compositeFields =
    Artist
      <$> lmap (.name) (field primitive d0 nonNullable)
      <*> lmap (.genre) (field enum d0 nonNullable)

columns :: Columns (Timestamptz, Maybe (Vector Int8), Vector (Vector (Maybe Artist)))
columns =
  (,,)
    <$> column primitive d0 nonNullable
    <*> column primitive (d1 nonNullable vector) nullable
    <*> column composite (d2 nullable vector vector) nonNullable
