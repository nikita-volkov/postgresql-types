module Demo.Types.Artist where

import Demo.Types.Genre (Genre)
import PostgresqlTypes.Mapping
import Prelude hiding (Int8, Text)

data Artist = Artist
  { name :: Text,
    genre :: Genre
  }

instance MapsToScalar Artist where
  scalarOf =
    composite
      ""
      "artist"
      ( Artist
          <$> lmap (.name) (field d0 nonNullable)
          <*> lmap (.genre) (field d0 nonNullable)
      )
