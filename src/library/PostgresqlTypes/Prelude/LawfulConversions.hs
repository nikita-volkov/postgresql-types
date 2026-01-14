-- | Extras for the lawful-conversions library.
module PostgresqlTypes.Prelude.LawfulConversions where

import Data.Either
import Data.Maybe
import LawfulConversions

-- |
-- Like 'maybeFrom', but returns an 'Either' with the provided error on failure.
--
-- Useful for raising custom errors when conversion fails.
tryFrom :: (IsSome a b) => e -> a -> Either e b
tryFrom err a =
  case maybeFrom a of
    Just b -> Right b
    Nothing -> Left err
