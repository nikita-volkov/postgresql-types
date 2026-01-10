module PqProcedures.Procedures.GetTypeInfoByName
  ( GetTypeInfoByNameParams (..),
    GetTypeInfoByNameResult (..),
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
            Left err -> fail ("Failed to read OID from database, data: " <> show bs)
            Right value -> pure (Just value)
      where
        decoder = PtrPeeker.fixed PtrPeeker.beUnsignedInt4
