module Main.Scripts where

import Control.Monad
import qualified Data.Attoparsec.Text
import Data.Function
import Data.Maybe
import Data.Proxy
import Data.Tagged
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified PostgresqlTypes.Algebra
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
  (HasCallStack, QuickCheck.Arbitrary a, Show a, Eq a, PostgresqlTypes.Algebra.IsScalar a) =>
  Proxy a ->
  SpecWith Pq.Connection
mappingSpec _ =
  let typeName = untag (PostgresqlTypes.Algebra.typeName @a)
      maybeBaseOid = untag (PostgresqlTypes.Algebra.baseOid @a)
      maybeArrayOid = untag (PostgresqlTypes.Algebra.arrayOid @a)
      binEnc = PostgresqlTypes.Algebra.binaryEncoder @a
      binDec = PostgresqlTypes.Algebra.binaryDecoder @a
      txtEnc = PostgresqlTypes.Algebra.textualEncoder @a
      txtDec = PostgresqlTypes.Algebra.textualDecoder @a

      getOids connection = do
        case (maybeBaseOid, maybeArrayOid) of
          (Just baseOid, Just arrayOid) ->
            pure (baseOid, arrayOid)
          _ -> do
            Procedures.GetTypeInfoByNameResult {..} <- Procedures.run connection Procedures.GetTypeInfoByNameParams {name = typeName}
            baseOid <- case maybeBaseOid of
              Just oid -> pure oid
              Nothing -> case baseOid of
                Just oid -> pure oid
                Nothing -> fail $ "Base OID not found for type: " <> Text.unpack typeName
            arrayOid <- case maybeArrayOid of
              Just oid -> pure oid
              Nothing -> case arrayOid of
                Just oid -> pure oid
                Nothing -> fail $ "Array OID not found for type: " <> Text.unpack typeName
            pure (baseOid, arrayOid)
   in describe "IsScalar" do
        describe (Text.unpack typeName) do
          describe "Encoding via textualEncoder" do
            describe "And decoding via textualDecoder" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: a) -> do
                  let valueText = TextBuilder.toText (txtEnc value)
                  QuickCheck.idempotentIOProperty do
                    (baseOid, _) <- getOids connection
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = baseOid,
                            paramEncoding = Text.Encoding.encodeUtf8 valueText,
                            paramFormat = Pq.Text,
                            resultFormat = Pq.Text,
                            typeSignature = Nothing
                          }
                    let serverProducedText = Text.Encoding.decodeUtf8 bytes
                        decoding = Data.Attoparsec.Text.parseOnly txtDec serverProducedText
                    pure
                      ( QuickCheck.counterexample
                          ( Text.unpack
                              ( Text.intercalate
                                  "\n"
                                  [ "serverProducedText: " <> serverProducedText,
                                    "encoded: " <> valueText
                                  ]
                              )
                          )
                          (decoding === Right value)
                      )

            describe "And decoding via binaryDecoder" do
              it "Should produce the original value" \(connection :: Pq.Connection) ->
                QuickCheck.property \(value :: a) -> do
                  let valueText = TextBuilder.toText (txtEnc value)
                  QuickCheck.idempotentIOProperty do
                    (baseOid, _) <- getOids connection
                    bytes <-
                      Procedures.run
                        connection
                        Procedures.RunRoundtripQueryParams
                          { paramOid = baseOid,
                            paramEncoding = Text.Encoding.encodeUtf8 valueText,
                            paramFormat = Pq.Text,
                            resultFormat = Pq.Binary,
                            typeSignature = Nothing
                          }
                    let decoding = PtrPeeker.runVariableOnByteString binDec bytes
                    pure
                      ( QuickCheck.counterexample
                          ( Text.unpack
                              ( Text.intercalate
                                  "\n"
                                  [ "encoded: " <> valueText
                                  ]
                              )
                          )
                          (decoding === Right (Right value))
                      )

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
                            paramEncoding = PtrPoker.Write.toByteString (binEnc value),
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
                            paramEncoding = PtrPoker.Write.toByteString (binEnc value),
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
                            paramEncoding = PtrPoker.Write.toByteString (arrayBinEnc value),
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
              Procedures.GetTypeInfoByNameResult {..} <- Procedures.run connection Procedures.GetTypeInfoByNameParams {name = typeName}
              case (maybeBaseOid, maybeArrayOid) of
                (Just expectedBaseOid, Just expectedArrayOid) -> do
                  shouldBe baseOid (Just expectedBaseOid)
                  shouldBe arrayOid (Just expectedArrayOid)
                _ -> do
                  -- For types without stable OIDs, just verify the OIDs exist
                  shouldSatisfy baseOid isJust
                  shouldSatisfy arrayOid isJust
