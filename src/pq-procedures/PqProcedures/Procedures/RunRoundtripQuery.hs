module PqProcedures.Procedures.RunRoundtripQuery
  ( RunRoundtripQueryParams (..),
    RunRoundtripQueryResult (..),
  )
where

import qualified Data.ByteString as ByteString
import Data.Function
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Typeable
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import LawfulConversions
import qualified PostgresqlTypes.Primitive.Algebra as PostgresqlTypes.Primitive
import PqProcedures.Algebra
import PqProcedures.Procedures.RunStatement
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import qualified TestcontainersPostgresql
import TextBuilder (TextBuilder)
import qualified TextBuilder
import TextBuilderLawfulConversions ()
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
