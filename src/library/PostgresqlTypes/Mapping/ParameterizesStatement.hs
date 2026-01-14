module PostgresqlTypes.Mapping.ParameterizesStatement where

import qualified PostgresqlTypes.Codec as Codec
import PostgresqlTypes.Codec.Prelude

class ParameterizesStatement a where
  type ResultOf a
  statementOf :: StatementOf a

data StatementOf a = StatementOf
  { sql :: Text,
    params :: Codec.Params a,
    result :: Codec.Result (ResultOf a)
  }
