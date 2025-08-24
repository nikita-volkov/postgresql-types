module Main.Helpers where

import Control.Monad
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
import qualified PqProcedures as Procedures
import qualified PrimitiveLayer.Algebra as PrimitiveLayer
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck ((.&&.), (===))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import qualified TestcontainersPostgresql
import TextBuilder (TextBuilder)
import qualified TextBuilder
import TextBuilderLawfulConversions ()
import Prelude

withPqConnection :: SpecWith Pq.Connection -> Spec
withPqConnection =
  aroundAll \action -> do
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
      arrayOid = untag (PrimitiveLayer.arrayOid @a)
      binEnc = PrimitiveLayer.binaryEncoder @a
      binDec = PrimitiveLayer.binaryDecoder @a
      txtEnc = PrimitiveLayer.textualEncoder @a

  describe "Encoding via textualEncoder" do
    describe "And decoding via binaryDecoder" do
      it "Should produce the original value" \(connection :: Pq.Connection) ->
        QuickCheck.property \(value :: a) -> do
          QuickCheck.idempotentIOProperty do
            bytes <-
              Procedures.run
                connection
                Procedures.RunRoundtripQueryParams
                  { paramOid = baseOid,
                    paramEncoding = Text.Encoding.encodeUtf8 (TextBuilder.toText (txtEnc value)),
                    paramFormat = Pq.Text,
                    resultFormat = Pq.Binary
                  }
            let decoding = PeekyBlinders.decodeByteStringDynamically binDec bytes
            pure (decoding === Right (Right value))

  describe "Encoding via binaryEncoder" do
    describe "And decoding via binaryDecoder" do
      it "Should produce the original value" \(connection :: Pq.Connection) ->
        QuickCheck.property \(value :: a) -> do
          QuickCheck.idempotentIOProperty do
            bytes <-
              Procedures.run
                connection
                Procedures.RunRoundtripQueryParams
                  { paramOid = baseOid,
                    paramEncoding = PtrPoker.Write.writeToByteString (binEnc value),
                    paramFormat = Pq.Binary,
                    resultFormat = Pq.Binary
                  }
            let decoding = PeekyBlinders.decodeByteStringDynamically binDec bytes
            pure (decoding === Right (Right value))

    describe "As array" do
      let arrayElementBinEnc value =
            let write = binEnc value
             in PtrPoker.Write.bInt32 (fromIntegral (PtrPoker.Write.writeSize write)) <> write
          arrayBinEnc value =
            mconcat
              [ PtrPoker.Write.bWord32 1,
                PtrPoker.Write.bWord32 0,
                PtrPoker.Write.bWord32 baseOid,
                PtrPoker.Write.bInt32 (fromIntegral (length value)),
                PtrPoker.Write.bWord32 1,
                foldMap arrayElementBinEnc value
              ]
          arrayElementBinDec = do
            size <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
            case size of
              -1 -> error "TODO"
              _ -> PeekyBlinders.forceSize (fromIntegral size) binDec
          arrayBinDec = do
            dims <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            _ <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            baseOid <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
            case dims of
              0 -> pure (baseOid, Right [])
              1 -> do
                length <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
                _ <- PeekyBlinders.statically PeekyBlinders.beUnsignedInt4
                values <- replicateM (fromIntegral length) arrayElementBinDec
                pure (baseOid, sequence values)
              _ -> error "Bug"
      describe "And decoding via binaryDecoder" do
        it "Should produce the original value" \(connection :: Pq.Connection) ->
          QuickCheck.property \(value :: [a]) -> do
            QuickCheck.idempotentIOProperty do
              bytes <-
                Procedures.run
                  connection
                  Procedures.RunRoundtripQueryParams
                    { paramOid = arrayOid,
                      paramEncoding = PtrPoker.Write.writeToByteString (arrayBinEnc value),
                      paramFormat = Pq.Binary,
                      resultFormat = Pq.Binary
                    }
              let decoding = PeekyBlinders.decodeByteStringDynamically arrayBinDec bytes
              (decodedBaseOid, decoding) <- case decoding of
                Left bytesNeeded -> fail $ "More input bytes needed: " <> show bytesNeeded
                Right decoding -> pure decoding
              decodedValue <- case decoding of
                Right value -> pure value
                _ -> fail "Decoding failed 2"
              pure (decodedBaseOid === baseOid .&&. decodedValue === value)

  describe "Metadata" do
    it "Should match the DB catalogue" \(connection :: Pq.Connection) -> do
      result <- Procedures.run connection Procedures.GetTypeInfoByNameParams {name = typeName}
      shouldBe (Just baseOid) result.baseOid
      shouldBe (Just arrayOid) result.arrayOid
