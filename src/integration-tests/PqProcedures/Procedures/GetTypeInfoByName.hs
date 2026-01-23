module PqProcedures.Procedures.GetTypeInfoByName
  ( GetTypeInfoByNameParams (..),
    GetTypeInfoByNameResult (..),
  )
where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import PqProcedures.Algebra
import PqProcedures.Procedures.RunStatement
import qualified PtrPeeker
import Prelude

newtype GetTypeInfoByNameParams = GetTypeInfoByNameParams
  { name :: Text
  }

data GetTypeInfoByNameResult = GetTypeInfoByNameResult
  { baseOid :: Maybe Word32,
    arrayOid :: Maybe Word32
  }

instance Procedure GetTypeInfoByNameParams GetTypeInfoByNameResult where
  run connection GetTypeInfoByNameParams {name} = do
    run
      connection
      RunStatementParams
        { sql =
            "SELECT t.oid AS base_oid, t.typarray AS array_oid\n\
            \FROM pg_type t\n\
            \WHERE t.typname = $1\n\
            \LIMIT 1",
          params =
            [ Just
                ( 25, -- OID of TEXT
                  Text.Encoding.encodeUtf8 name,
                  Pq.Binary
                )
            ],
          resultFormat = Pq.Binary
        }
      >>= processPqResult

processPqResult :: Pq.Result -> IO GetTypeInfoByNameResult
processPqResult result = do
  baseOid <- readOid 0 0
  arrayOid <- readOid 0 1
  pure GetTypeInfoByNameResult {baseOid, arrayOid}
  where
    readOid x y =
      Pq.getvalue result x y >>= \case
        Nothing -> pure Nothing
        Just bs ->
          case PtrPeeker.runVariableOnByteString decoder bs of
            Left _err -> fail ("Failed to read OID from database, data: " <> show bs)
            Right value -> pure (Just value)
      where
        decoder = PtrPeeker.fixed PtrPeeker.beUnsignedInt4
