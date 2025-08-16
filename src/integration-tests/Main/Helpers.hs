module Main.Helpers where

import qualified Data.ByteString as ByteString
import Data.Function
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Tagged (untag)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified PeekyBlinders
import qualified PrimitiveLayer
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import qualified TestcontainersPostgresql
import qualified TextBuilder
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

mappingSpec ::
  forall a.
  (QuickCheck.Arbitrary a, Show a, Eq a, PrimitiveLayer.Primitive a) =>
  Proxy a ->
  SpecWith Pq.Connection
mappingSpec _ = do
  let typeName = untag (PrimitiveLayer.typeName @a)
      baseOid = untag (PrimitiveLayer.baseOid @a)
      binEnc = PrimitiveLayer.binaryEncoder @a
      binDec = PrimitiveLayer.binaryDecoder @a
      txtEnc = PrimitiveLayer.textualEncoder @a
  describe ("With mapping of base type " <> Text.unpack typeName) do
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
  Int32 ->
  ByteString.ByteString ->
  Pq.Format ->
  Pq.Format ->
  IO ByteString.ByteString
runRoundtripQuery connection paramOid paramEncoding paramFormat resultFormat = do
  result <-
    Pq.execParams
      connection
      "select $1"
      [ Just
          ( Pq.Oid (fromIntegral paramOid),
            paramEncoding,
            paramFormat
          )
      ]
      resultFormat
  result <- case result of
    Nothing -> do
      m <- Pq.errorMessage connection
      fail ("execParams produced no result due to: " <> show m)
    Just result -> pure result
  bytes <- Pq.getvalue result 0 0
  bytes <- case bytes of
    Nothing -> fail "getvalue produced no bytes"
    Just bytes -> pure bytes
  pure bytes
