module PqProcedures.RunRoundtripQuery where

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
import qualified PeekyBlinders
import qualified PqProcedures.RunStatement
import qualified PrimitiveLayer.Algebra as PrimitiveLayer
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

data Params = Params
  { paramOid :: Word32,
    paramEncoding :: ByteString.ByteString,
    paramFormat :: Pq.Format,
    resultFormat :: Pq.Format
  }

type Result = ByteString.ByteString

run :: Pq.Connection -> Params -> IO Result
run connection (Params {..}) = do
  pqResult <-
    PqProcedures.RunStatement.run
      connection
      PqProcedures.RunStatement.Params
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
