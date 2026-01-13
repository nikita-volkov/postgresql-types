{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Main (main) where

import Data.Tagged
import PostgresqlTypes.Codec
import PostgresqlTypes.Primitive
import Prelude hiding (Int8, Text)

main :: IO ()
main = error "TODO"

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
