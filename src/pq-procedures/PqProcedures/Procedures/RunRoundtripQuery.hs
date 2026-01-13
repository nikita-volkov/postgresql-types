module PqProcedures.Procedures.RunRoundtripQuery
  ( RunRoundtripQueryParams (..),
    RunRoundtripQueryResult,
  )
where

import qualified Data.ByteString as ByteString
import Data.Maybe
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import PqProcedures.Algebra
import PqProcedures.Procedures.RunStatement
import Prelude

data RunRoundtripQueryParams = RunRoundtripQueryParams
  { paramOid :: Word32,
    paramEncoding :: ByteString.ByteString,
    paramFormat :: Pq.Format,
    resultFormat :: Pq.Format
  }

type RunRoundtripQueryResult = ByteString.ByteString

instance Procedure RunRoundtripQueryParams RunRoundtripQueryResult where
  run connection (RunRoundtripQueryParams {..}) = do
    pqResult <-
      run
        connection
        RunStatementParams
          { sql = "SELECT $1",
            params =
              [ Just (paramOid, paramEncoding, paramFormat)
              ],
            resultFormat = resultFormat
          }
    bytes <- Pq.getvalue pqResult 0 0
    case bytes of
      Nothing -> fail "getvalue produced no bytes"
      Just bytes -> pure bytes
