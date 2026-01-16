module Main.Scripts where

import Control.Monad
import qualified Data.Attoparsec.Text
import qualified Data.ByteString as ByteString
import Data.Function
import Data.Maybe
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import Data.Typeable
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import LawfulConversions
import qualified PostgresqlTypes as PostgresqlTypes
import qualified PqProcedures as Procedures
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck ((.&&.), (===))
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import Prelude

mappingSpec ::
  forall a.
  (HasCallStack, QuickCheck.Arbitrary a, Show a, Eq a, PostgresqlTypes.IsStandardType a) =>
  Proxy a ->
  SpecWith Pq.Connection
mappingSpec _ =
  let typeName = untag (PostgresqlTypes.typeName @a)
      maybeBaseOid = untag (PostgresqlTypes.baseOid @a)
      maybeArrayOid = untag (PostgresqlTypes.arrayOid @a)
      binEnc = PostgresqlTypes.binaryEncoder @a
      binDec = PostgresqlTypes.binaryDecoder @a
      txtEnc = PostgresqlTypes.textualEncoder @a
      txtDec = PostgresqlTypes.textualDecoder @a

      getOids connection = do
        case (maybeBaseOid, maybeArrayOid) of
          (Just baseOid, Just arrayOid) ->
            pure (baseOid, arrayOid)
          _ -> do
            result <- Procedures.run connection Procedures.GetTypeInfoByNameParams {name = typeName}
            let baseOid = case maybeBaseOid of
                  Just oid -> oid
                  Nothing -> fromMaybe (error $ "Base OID not found for type: " <> to typeName) result.baseOid
                arrayOid = case maybeArrayOid of
                  Just oid -> oid
                  Nothing -> fromMaybe (error $ "Array OID not found for type: " <> to typeName) result.arrayOid
            pure (baseOid, arrayOid)
   in describe "IsStandardType" do
        describe (to typeName) do
          describe "Encoding via textualEncoder" do
            describe "And decoding via textualDecoder" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: a) -> do
                  QuickCheck.idempotentIOProperty do
                    (baseOid, _) <- getOids connection
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = baseOid,
                            paramEncoding = Text.Encoding.encodeUtf8 (TextBuilder.toText (txtEnc value)),
                            paramFormat = Pq.Text,
                            resultFormat = Pq.Text,
                            typeSignature = Nothing
                          }
                    let serverProducedText = Text.Encoding.decodeUtf8 bytes
                        decoding = Data.Attoparsec.Text.parseOnly txtDec serverProducedText
                    pure
                      ( QuickCheck.counterexample
                          ("serverProducedText: " <> show serverProducedText)
                          (decoding === Right value)
                      )

            describe "And decoding via binaryDecoder" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: a) -> do
                  QuickCheck.idempotentIOProperty do
                    (baseOid, _) <- getOids connection
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = baseOid,
                            paramEncoding = Text.Encoding.encodeUtf8 (TextBuilder.toText (txtEnc value)),
                            paramFormat = Pq.Text,
                            resultFormat = Pq.Binary,
                            typeSignature = Nothing
                          }
                    let decoding = PtrPeeker.runVariableOnByteString binDec bytes
                    pure (decoding === Right (Right value))

          describe "Encoding via binaryEncoder" do
            describe "And decoding via textualDecoder" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: a) -> do
                  QuickCheck.idempotentIOProperty do
                    (baseOid, _) <- getOids connection
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = baseOid,
                            paramEncoding = PtrPoker.Write.writeToByteString (binEnc value),
                            paramFormat = Pq.Binary,
                            resultFormat = Pq.Text,
                            typeSignature = Nothing
                          }
                    let serverProducedText = Text.Encoding.decodeUtf8 bytes
                        decoding = Data.Attoparsec.Text.parseOnly txtDec serverProducedText
                    pure
                      ( QuickCheck.counterexample
                          ("serverProducedText: " <> show serverProducedText)
                          (decoding === Right value)
                      )

            describe "And decoding via binaryDecoder" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: a) -> do
                  QuickCheck.idempotentIOProperty do
                    (baseOid, _) <- getOids connection
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = baseOid,
                            paramEncoding = PtrPoker.Write.writeToByteString (binEnc value),
                            paramFormat = Pq.Binary,
                            resultFormat = Pq.Binary,
                            typeSignature = Nothing
                          }
                    let decoding = PtrPeeker.runVariableOnByteString binDec bytes
                    pure (decoding === Right (Right value))

            describe "As array" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: [a]) -> do
                  QuickCheck.idempotentIOProperty do
                    (baseOid, arrayOid) <- getOids connection
                    let arrayElementBinEnc value' =
                          let write = binEnc value'
                           in PtrPoker.Write.bInt32 (fromIntegral (PtrPoker.Write.writeSize write)) <> write
                        arrayBinEnc value' =
                          mconcat
                            [ PtrPoker.Write.bWord32 1,
                              PtrPoker.Write.bWord32 0,
                              PtrPoker.Write.bWord32 baseOid,
                              PtrPoker.Write.bInt32 (fromIntegral (length value')),
                              PtrPoker.Write.bWord32 1,
                              foldMap arrayElementBinEnc value'
                            ]
                        arrayElementBinDec = do
                          size <- PtrPeeker.fixed PtrPeeker.beSignedInt4
                          case size of
                            -1 -> error "TODO"
                            _ -> PtrPeeker.forceSize (fromIntegral size) binDec
                        arrayBinDec = do
                          dims <- PtrPeeker.fixed PtrPeeker.beUnsignedInt4
                          _ <- PtrPeeker.fixed PtrPeeker.beUnsignedInt4
                          decodedBaseOid <- PtrPeeker.fixed PtrPeeker.beUnsignedInt4
                          case dims of
                            0 -> pure (decodedBaseOid, Right [])
                            1 -> do
                              length <- PtrPeeker.fixed PtrPeeker.beUnsignedInt4
                              _ <- PtrPeeker.fixed PtrPeeker.beUnsignedInt4
                              values <- replicateM (fromIntegral length) arrayElementBinDec
                              pure (decodedBaseOid, sequence values)
                            _ -> error "Bug"
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = arrayOid,
                            paramEncoding = PtrPoker.Write.writeToByteString (arrayBinEnc value),
                            paramFormat = Pq.Binary,
                            resultFormat = Pq.Binary,
                            typeSignature = Nothing
                          }
                    let decoding = PtrPeeker.runVariableOnByteString arrayBinDec bytes
                    (decodedBaseOid, decoding) <- case decoding of
                      Left bytesNeeded -> fail $ "More input bytes needed: " <> show bytesNeeded
                      Right decoding -> pure decoding
                    decodedValue <- case decoding of
                      Right value' -> pure value'
                      _ -> fail "Decoding failed 2"
                    pure (decodedBaseOid === baseOid .&&. decodedValue === value)

          describe "Metadata" do
            it "Should match the DB catalogue" \(connection :: Pq.Connection) -> do
              result <- Procedures.run connection Procedures.GetTypeInfoByNameParams {name = typeName}
              case (maybeBaseOid, maybeArrayOid) of
                (Just expectedBaseOid, Just expectedArrayOid) -> do
                  shouldBe (Just expectedBaseOid) result.baseOid
                  shouldBe (Just expectedArrayOid) result.arrayOid
                _ -> do
                  -- For types without stable OIDs, just verify the OIDs exist
                  shouldSatisfy result.baseOid isJust
                  shouldSatisfy result.arrayOid isJust
