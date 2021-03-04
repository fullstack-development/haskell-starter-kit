module AppName.Gateways.Database.Setup
  ( withDbPool,
    withDbPoolDebug,
    withLoggedDbPool,
  )
where

import AppName.Gateways.Database.Connection (createPgConnString)
import qualified Config as C
import Control.Exception.Safe (MonadMask, bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    NoLoggingT,
    runNoLoggingT,
    runStdoutLoggingT,
  )
import Data.Functor (($>))
import Data.Pool (Pool, destroyAllResources)
import Database.Persist.Postgresql (SqlBackend, createPostgresqlPoolModified)
import Database.PostgreSQL.Simple (execute_)

withDbPoolDebug :: C.Config -> (Pool SqlBackend -> LoggingT IO a) -> IO a
withDbPoolDebug = withLoggedDbPool runStdoutLoggingT

withDbPool :: C.Config -> (Pool SqlBackend -> NoLoggingT IO a) -> IO a
withDbPool = withLoggedDbPool runNoLoggingT

withLoggedDbPool ::
  (MonadUnliftIO m, MonadLogger m, MonadMask m) =>
  (m a -> IO a) ->
  C.Config ->
  (Pool SqlBackend -> m a) ->
  IO a
withLoggedDbPool runLogging config action = do
  poolLimit <- C.getPoolLimit config
  connStr <- createPgConnString config
  liftIO $
    runLogging $
      bracket
        (createPostgresqlPoolModified setupDbConnection connStr poolLimit)
        releasePool
        action
  where
    releasePool = liftIO . destroyAllResources
    setupDbConnection conn =
      execute_ conn "set search_path to users, public;" $> ()
