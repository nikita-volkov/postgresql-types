module Main.Helpers where

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

withPqConnection :: (Pq.Connection -> IO ()) -> IO ()
withPqConnection action = do
  TestcontainersPostgresql.with False \(host, port) -> do
    connection <- Pq.connectdb (connectionString host port)
    status <- Pq.status connection
    case status of
      Pq.ConnectionOk -> return ()
      _ -> do
        message <- Pq.errorMessage connection
        fail ("Failed to connect to database: " <> show message)
    _ <-
      Pq.exec
        connection
        "SET client_min_messages TO WARNING;\n\
        \SET client_encoding = 'UTF8';\n\
        \SET intervalstyle = 'postgres';"
    result <- action connection
    Pq.finish connection
    pure result
  where
    connectionString host port =
      ByteString.intercalate " " components
      where
        components =
          [ "host=" <> (Text.Encoding.encodeUtf8 host),
            "port=" <> (fromString . show) port,
            "user=" <> user,
            "password=" <> password,
            "dbname=" <> db
          ]
          where
            user = "postgres"
            password = "postgres"
            db = "postgres"

withType :: forall a. (Typeable a) => [Proxy a -> SpecWith Pq.Connection] -> SpecWith Pq.Connection
withType specs = do
  describe (show (typeOf (undefined :: a))) do
    mapM_ (\spec -> spec Proxy) specs

mappingSpec ::
  forall a.
  (QuickCheck.Arbitrary a, Show a, Eq a, PrimitiveLayer.Mapping a) =>
  Proxy a ->
  SpecWith Pq.Connection
mappingSpec _ = describe "Mapping" do
  let typeName = untag (PrimitiveLayer.typeName @a)
      baseOid = untag (PrimitiveLayer.baseOid @a)
      binEnc = PrimitiveLayer.binaryEncoder @a
      binDec = PrimitiveLayer.binaryDecoder @a
      txtEnc = PrimitiveLayer.textualEncoder @a

  describe "Encoding via textualEncoder" do
    describe "And decoding via binaryDecoder" do
      it "Should produce the original value" \(connection :: Pq.Connection) ->
        QuickCheck.property \(value :: a) -> do
          QuickCheck.idempotentIOProperty do
            bytes <-
              runRoundtripQuery
                connection
                baseOid
                (Text.Encoding.encodeUtf8 (TextBuilder.toText (txtEnc value)))
                Pq.Text
                Pq.Binary
            let decoding = PeekyBlinders.decodeByteStringDynamically binDec bytes
            pure (decoding === Right (Right value))

  describe "Encoding via binaryEncoder" do
    describe "And decoding via binaryDecoder" do
      it "Should produce the original value" \(connection :: Pq.Connection) ->
        QuickCheck.property \(value :: a) -> do
          QuickCheck.idempotentIOProperty do
            bytes <-
              runRoundtripQuery
                connection
                baseOid
                (PtrPoker.Write.writeToByteString (binEnc value))
                Pq.Binary
                Pq.Binary
            let decoding = PeekyBlinders.decodeByteStringDynamically binDec bytes
            pure (decoding === Right (Right value))

runRoundtripQuery ::
  Pq.Connection ->
  Word32 ->
  ByteString.ByteString ->
  Pq.Format ->
  Pq.Format ->
  IO ByteString.ByteString
runRoundtripQuery connection paramOid paramEncoding paramFormat resultFormat = do
  let params =
        [ Just
            ( paramOid,
              paramEncoding,
              paramFormat
            )
        ]
  result <- runStatement connection "select $1" params resultFormat
  bytes <- Pq.getvalue result 0 0
  bytes <- case bytes of
    Nothing -> fail "getvalue produced no bytes"
    Just bytes -> pure bytes
  pure bytes

runStatement ::
  Pq.Connection ->
  Text.Text ->
  -- | Params
  [ Maybe
      ( Word32,
        ByteString.ByteString,
        Pq.Format
      )
  ] ->
  -- | Result format.
  Pq.Format ->
  IO Pq.Result
runStatement connection sql params resultFormat = do
  result <-
    Pq.execParams
      connection
      (Text.Encoding.encodeUtf8 sql)
      (fmap (fmap (\(oid, encoding, format) -> (Pq.Oid (fromIntegral oid), encoding, format))) params)
      resultFormat
  result <- case result of
    Nothing -> do
      m <- Pq.errorMessage connection
      failWithSql "No result" (onto (show m))
    Just result -> pure result
  resultErrorField <- Pq.resultErrorField result Pq.DiagMessagePrimary
  case resultErrorField of
    Nothing -> pure ()
    Just err -> failWithSql "Error field present" (onto (show err))
  pure result
  where
    failWithSql :: Text -> Text -> IO a
    failWithSql msg reason =
      fail (to @_ @TextBuilder (from @Text msg <> "\nDue to:\n\t\t" <> from @Text reason <> "\nQuery:\n\t\t" <> from @Text sql))
