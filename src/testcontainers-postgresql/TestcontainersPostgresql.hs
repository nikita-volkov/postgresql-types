module TestcontainersPostgresql
  ( with,
  )
where

import Data.Function
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified TestContainers
import qualified TestContainers.Hspec
import Prelude

containerRequest :: Bool -> TestContainers.ContainerRequest
containerRequest forwardLogs =
  TestContainers.containerRequest (TestContainers.fromTag "postgres:17")
    & TestContainers.setExpose [5432]
    & TestContainers.setWaitingFor waitUntilReady
    & TestContainers.setEnv [("POSTGRES_PASSWORD", "postgres")]
    & (if forwardLogs then TestContainers.withFollowLogs TestContainers.consoleLogConsumer else id)

waitUntilReady :: TestContainers.WaitUntilReady
waitUntilReady =
  mconcat
    [ TestContainers.waitForLogLine TestContainers.Stderr (Text.Lazy.isInfixOf "database system is ready to accept connections"),
      TestContainers.waitUntilMappedPortReachable 5432
    ]

setup :: (TestContainers.MonadDocker m) => Bool -> m (Text, Int)
setup forwardLogs = do
  container <- TestContainers.run (containerRequest forwardLogs)
  pure $ TestContainers.containerAddress container 5432

with :: Bool -> ((Text, Int) -> IO ()) -> IO ()
with forwardLogs = do
  TestContainers.Hspec.withContainers (setup forwardLogs)
