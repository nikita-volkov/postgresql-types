module PostgresqlTypes.Mapping where

import qualified PostgresqlTypes.Codec as Codec
import PostgresqlTypes.Codec.Prelude

class IsStaticStatement a where
  type ResultOf a
  staticStatementSql :: Tagged a Text
  staticStatementParams :: Codec.Params a
  staticStatementResult :: Tagged a (Codec.Result (ResultOf a))
