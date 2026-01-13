{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlTypes.Codec.Params where

import PostgresqlTypes.Codec.Dimensionality
import PostgresqlTypes.Codec.Nullability
import PostgresqlTypes.Codec.Prelude
import PostgresqlTypes.Codec.Scalar
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Parameterized query parameters encoder.
data Params a = Params
  { count :: Int32,
    binaryEncoder :: [a -> Maybe Write.Write],
    textualEncoder :: [a -> TextBuilder.TextBuilder]
  }

instance Contravariant Params

instance Semigroup (Params a)

instance Monoid (Params a)

param :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
param scalar dimensionality nullability = error "TODO"
