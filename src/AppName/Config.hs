module AppName.Config
  ( Config,
    C.require,
    C.lookup,
    retrieveConfig,
    getKeysFilePath,
    getPort,
    getPoolLimit,
    getLoggerConfig,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Persist.Postgresql (SqlPersistT)
import Ext.Logger.Colog (Severity (Debug))
import Ext.Logger.Config (LoggerConfig (..))
import Text.Read (readMaybe)
import UnliftIO.Exception (throwString)

type Config = C.Config

retrieveConfig :: MonadIO m =>  m C.Config
retrieveConfig = do
  let configPath = "./config/local.conf"
  liftIO $ C.load [C.Required configPath]

getKeysFilePath :: MonadIO m => C.Config -> m FilePath
getKeysFilePath config = liftIO $ C.require config "auth.key_path"

getPort :: MonadIO m => C.Config -> m Int
getPort config = liftIO $ C.require config "web_server.port"

getPoolLimit :: MonadIO m => C.Config -> m Int
getPoolLimit config = liftIO $ C.require config "database.pool_limit"

getLoggerConfig :: MonadIO m => C.Config -> m LoggerConfig
getLoggerConfig config = liftIO $ do
  appInstanceName <- C.require config "log.app_instance_name"
  logToStdout <- C.require config "log.log_to_stdout"
  logLevelRaw <- C.require config "log.log_level"
  pure $
    LoggerConfig
      { appInstanceName = appInstanceName,
        logToStdout = logToStdout,
        logLevel = fromMaybe Debug (readMaybe logLevelRaw)
      }
