module PqProcedures.Procedures.RunStatement
  ( RunStatementParams (..),
    RunStatementResult,
  )
where

import qualified Data.ByteString as ByteString
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import LawfulConversions
import PqProcedures.Algebra
import TextBuilder (TextBuilder)
import TextBuilderLawfulConversions ()
import Prelude

data RunStatementParams = RunStatementParams
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

type RunStatementResult = Pq.Result

instance Procedure RunStatementParams RunStatementResult where
  run connection (RunStatementParams sql params resultFormat) = do
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
        fail (from @TextBuilder (from @Text msg <> "\nDue to:\n\t\t" <> from @Text reason <> "\nQuery:\n\t\t" <> from @Text sql))
