module PostgresqlTypes.Mapping.ParameterizesStatement where

import qualified PostgresqlTypes.Codec as Codec
import PostgresqlTypes.Codec.Prelude

class ParameterizesStatement a where
  type ResultOf a
  statementSql :: Tagged a Text
  statementParams :: Codec.Params a
  statementResult :: Tagged a (Codec.Result (ResultOf a))
