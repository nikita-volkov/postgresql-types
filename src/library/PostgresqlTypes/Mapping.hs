module PostgresqlTypes.Mapping where

import qualified Data.Vector as Vector
import qualified PostgresqlTypes.Codec.Algebra as Codec
import qualified PostgresqlTypes.Codec.Algebra.Writes as Writes
import PostgresqlTypes.Codec.Prelude
import qualified PostgresqlTypes.Primitive.Algebra as Primitive
import qualified PostgresqlTypes.Primitive.Types as Primitive
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

class IsStaticStatement a where
  type ResultOf a
  staticStatementSql :: Tagged a Text
  staticStatementParams :: Codec.Params a
  staticStatementResult :: Tagged a (Codec.Result (ResultOf a))
