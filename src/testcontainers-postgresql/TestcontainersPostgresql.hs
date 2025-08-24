module TestcontainersPostgresql
  ( Config (..),
    Distro (..),
    run,
    tag,
  )
where

import Data.Function
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified TestContainers
import qualified TestContainers.Hspec
import Prelude

data Config = Config
  { forwardLogs :: Bool,
    distro :: Distro
  }

data Distro
  = Distro17
  | Distro9
  deriving stock (Show, Eq)

tag :: Distro -> Text
tag Distro17 = "postgres:17"
tag Distro9 = "postgres:9"

containerRequest :: Config -> TestContainers.ContainerRequest
containerRequest (Config forwardLogs distro) =
  TestContainers.containerRequest (TestContainers.fromTag (tag distro))
    & TestContainers.setExpose [5432]
    & TestContainers.setWaitingFor waitUntilReady
    & TestContainers.setEnv [("POSTGRES_HOST_AUTH_METHOD", "trust")]
    & (if forwardLogs then TestContainers.withFollowLogs TestContainers.consoleLogConsumer else id)

waitUntilReady :: TestContainers.WaitUntilReady
waitUntilReady =
  mconcat
    [ TestContainers.waitForLogLine TestContainers.Stderr (Text.Lazy.isInfixOf "database system is ready to accept connections"),
      TestContainers.waitUntilMappedPortReachable 5432
    ]

setup :: (TestContainers.MonadDocker m) => Config -> m (Text, Int)
setup config = do
  container <- TestContainers.run (containerRequest config)
  pure $ TestContainers.containerAddress container 5432

-- | Run a session on a PostgreSQL container on the scope of a host and a port.
run :: Config -> ((Text, Int) -> IO ()) -> IO ()
run config = do
  TestContainers.Hspec.withContainers (setup config)
