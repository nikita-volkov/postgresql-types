module PqProcedures.GetTypeInfoByName where

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
import qualified PqProcedures.RunStatement as RunStatement
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

type Params = Text

data Result = Result
  { baseOid :: Maybe Word32,
    arrayOid :: Maybe Word32
  }

run :: Pq.Connection -> Params -> IO Result
run connection name =
  RunStatement.run
    connection
    RunStatement.Params
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
    >>= processResult

processResult :: Pq.Result -> IO Result
processResult result = do
  baseOid <- readOid 0 0
  arrayOid <- readOid 0 1
  pure Result {baseOid, arrayOid}
  where
    readOid x y =
      Pq.getvalue result x y >>= \case
        Nothing -> pure Nothing
        Just bs ->
          case PeekyBlinders.decodeByteStringDynamically decoder bs of
            Left err -> fail ("Failed to read OID from database, data: " <> show bs)
            Right value -> pure (Just value)
      where
        decoder = PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
