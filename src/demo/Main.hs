{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Main (main) where

import Data.Tagged
import PostgresqlTypes.Mapping
import PostgresqlTypes.Primitive
import Prelude hiding (Int8, Text)

main :: IO ()
main = error "TODO"

data Genre = Rock | Pop | Jazz
  deriving (Eq, Ord)

instance MapsToScalar Genre where
  scalarOf =
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

instance MapsToScalar Artist where
  scalarOf =
    composite
      ""
      "artist"
      ( Artist
          <$> lmap (.name) (field d0 nonNullable)
          <*> lmap (.genre) (field d0 nonNullable)
      )

columns :: Columns (Timestamptz, Maybe (Vector Int8), Vector (Vector (Maybe Artist)))
columns =
  (,,)
    <$> column d0 nonNullable
    <*> column (d1 nonNullable vector) nullable
    <*> column (d2 nullable vector vector) nonNullable
