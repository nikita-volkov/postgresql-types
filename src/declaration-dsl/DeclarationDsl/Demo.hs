module DeclarationDsl.Demo where

import DeclarationDsl.Algebra
import DeclarationDsl.Prelude

data Genre = Rock | Pop | Jazz

genre :: Scalar Genre
genre =
  enum
    ""
    "genre"
    [ ("rock", Rock),
      ("pop", Pop),
      ("jazz", Jazz)
    ]

data Artist = Artist
  { name :: Text,
    genre :: Genre
  }

artist :: Scalar Artist
artist =
  composite
    ""
    "artist"
    ( Artist
        <$> lmap (.name) (field text d0 nonNullable)
        <*> lmap (.genre) (field genre d0 nonNullable)
    )

columns :: (Column UTCTime, Column (Maybe (Vector Int16)), Column (Vector (Vector (Maybe Artist))))
columns =
  ( column timestamptz d0 nonNullable,
    column int2 (d1 nonNullable vector) nullable,
    column artist (d2 nullable vector vector) nonNullable
  )
