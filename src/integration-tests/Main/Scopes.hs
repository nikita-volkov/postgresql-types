module Main.Scopes where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as ByteString
import Data.Function
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import Data.Typeable
import Data.Word
import qualified Database.PostgreSQL.LibPQ as Pq
import LawfulConversions
import qualified TestcontainersPostgresql
import qualified TextBuilder
import Test.Hspec
import Prelude

withContainer :: Text -> SpecWith (Text, Word16) -> Spec
withContainer tagName =
  describe (to tagName) . aroundAll (TestcontainersPostgresql.run config)
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          auth = TestcontainersPostgresql.TrustAuth,
          tagName
        }

withConnection :: Maybe Int -> SpecWith Pq.Connection -> SpecWith (Text, Word16)
withConnection extraFloatDigits =
  withConnectionPool 100 extraFloatDigits . withTQueueElement clean
  where
    clean connection = pure connection

withConnectionPool :: Int -> Maybe Int -> SpecWith (TQueue Pq.Connection) -> SpecWith (Text, Word16)
withConnectionPool poolSize extraFloatDigits =
  describe name . mapSubject connectionStringByContainerInfo . withPool poolSize acquire release
  where
    name =
      mconcat
        [ "extraFloatDigits: ",
          case extraFloatDigits of
            Just digits -> show digits
            Nothing -> "default"
        ]

    connectionStringByContainerInfo (host, port) =
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

    acquire connectionString = do
      connection <- Pq.connectdb connectionString
      status <- Pq.status connection
      case status of
        Pq.ConnectionOk -> pure ()
        _ -> do
          message <- Pq.errorMessage connection
          fail ("Failed to connect to database: " <> show message)
      result <-
        Pq.exec
          connection
          ( [ Just "SET client_min_messages TO WARNING",
              Just "SET client_encoding = 'UTF8'",
              Just "SET intervalstyle = 'iso_8601'",
              fmap
                (\digits -> "SET extra_float_digits = " <> TextBuilder.decimal digits)
                extraFloatDigits
            ]
              & catMaybes
              & fmap (<> ";")
              & TextBuilder.intercalate "\n"
              & TextBuilder.toText
              & Text.Encoding.encodeUtf8
          )
      case result of
        Just _ -> pure ()
        Nothing -> do
          message <- Pq.errorMessage connection
          fail ("Failed to set connection parameters: " <> show message)
      pure connection

    release = Pq.finish

withPool ::
  -- | Pool size.
  Int ->
  -- | Acquire.
  (b -> IO a) ->
  -- | Release.
  (a -> IO ()) ->
  SpecWith (TQueue a) ->
  SpecWith b
withPool poolSize acquire release =
  describe ("poolSize: " <> show poolSize) . aroundAllWith \actionWithQueue b ->
    bracket
      ( do
          queue <- newTQueueIO
          replicateConcurrently_ poolSize do
            element <- acquire b
            atomically $ writeTQueue queue element
          pure queue
      )
      ( \queue -> do
          replicateConcurrently_ poolSize do
            element <- atomically $ readTQueue queue
            release element
      )
      actionWithQueue

withTQueueElement ::
  -- | Clean. Called upon returning to the pool.
  (a -> IO a) ->
  SpecWith a ->
  SpecWith (TQueue a)
withTQueueElement clean =
  aroundWith \actionWithElement queue ->
    bracket
      (atomically $ readTQueue queue)
      ( \element -> do
          element <- clean element
          atomically $ writeTQueue queue element
      )
      actionWithElement

withType :: forall a b. (Typeable a) => [Proxy a -> SpecWith b] -> SpecWith b
withType specs = do
  describe (show (typeOf (undefined :: a))) do
    mapM_ (\spec -> spec Proxy) specs
