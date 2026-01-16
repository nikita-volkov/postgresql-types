module PqProcedures.Procedures.RunRoundtripQuery
  ( RunRoundtripQueryParams (..),
    RunRoundtripQueryResult,
  )
where

import qualified Data.ByteString as ByteString
import Data.Maybe
import qualified Data.Text as Text
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import PqProcedures.Algebra
import PqProcedures.Procedures.RunStatement
import Prelude

data RunRoundtripQueryParams = RunRoundtripQueryParams
  { paramOid :: Word32,
    paramEncoding :: ByteString.ByteString,
    paramFormat :: Pq.Format,
    resultFormat :: Pq.Format,
    typeSignature :: Maybe Text.Text
  }

type RunRoundtripQueryResult = ByteString.ByteString

instance Procedure RunRoundtripQueryParams RunRoundtripQueryResult where
  run connection (RunRoundtripQueryParams {..}) = do
    let sql = case typeSignature of
          Nothing -> "SELECT $1"
          Just sig -> "SELECT $1::" <> sig
    pqResult <-
      run
        connection
        RunStatementParams
          { sql = sql,
            params =
              [ Just (paramOid, paramEncoding, paramFormat)
              ],
            resultFormat = resultFormat
          }
    bytes <- Pq.getvalue pqResult 0 0
    case bytes of
      Nothing -> fail "getvalue produced no bytes"
      Just bytes -> pure bytes
