module PqProcedures.RunStatement where

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
  { sql :: Text.Text,
    -- | Params
    params ::
      [ Maybe
          ( Word32,
            ByteString.ByteString,
            Pq.Format
          )
      ],
    -- | Result format.
    resultFormat :: Pq.Format
  }

type Result = Pq.Result

run :: Pq.Connection -> Params -> IO Result
run connection (Params sql params resultFormat) = do
  result <-
    Pq.execParams
      connection
      (Text.Encoding.encodeUtf8 sql)
      (fmap (fmap (\(oid, encoding, format) -> (Pq.Oid (fromIntegral oid), encoding, format))) params)
      resultFormat
  result <- case result of
    Nothing -> do
      m <- Pq.errorMessage connection
      failWithSql "No result" (maybe "" onto m)
    Just result -> pure result
  resultErrorField <- Pq.resultErrorField result Pq.DiagMessagePrimary
  case resultErrorField of
    Nothing -> pure ()
    Just err -> failWithSql "Error field present" (onto err)
  pure result
  where
    failWithSql :: Text -> Text -> IO a
    failWithSql msg reason =
      fail (to @_ @TextBuilder (from @Text msg <> "\nDue to:\n\t\t" <> from @Text reason <> "\nQuery:\n\t\t" <> from @Text sql))
