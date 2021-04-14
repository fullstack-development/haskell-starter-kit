{-# LANGUAGE DeriveAnyClass #-}

module AppName.Config
  ( Config,
    C.require,
    C.lookup,
    retrieveConfig,
    getKeysFilePath,
    getPort,
    getPoolLimit,
    getLoggerConfig,
    AppConfig (..),
    loadConfig,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Dhall
import qualified Ext.Logger.Colog as Log (Severity (Debug))
import qualified Ext.Logger.Config as Log (LoggerConfig (..))
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data DbConfig = DbConfig
  { host :: Text,
    port :: Int,
    database :: Text,
    user :: Text,
    password :: Text,
    poolLimit :: Int
  }
  deriving (Generic, Dhall.FromDhall, Show)

data LogLevel = Debug | Info | Warning | Error
  deriving (Generic, Dhall.FromDhall, Show)

newtype AuthConfig = AuthConfig {pathToKey :: Text}
  deriving (Generic, Dhall.FromDhall, Show)

data LogToFile = NoLogToFile | AllowLogToFile Text 
  deriving (Generic, Dhall.FromDhall, Show)

data LogConfig = LogConfig
  { appName :: Text,
    logToStdout :: Bool,
    logLevel :: LogLevel,
    logRawSql :: Bool,
    logToFile :: LogToFile
  }
  deriving (Generic, Dhall.FromDhall, Show)

data AppConfig = AppConfig
  { authConfig :: AuthConfig,
    logConfig :: LogConfig,
    dbConfig :: DbConfig
  }
  deriving (Generic, Dhall.FromDhall, Show)

-- TODO load path to config file from ENV VAR
-- TODO use default dev config if it was not provided. Warn about using default config.
loadConfig :: MonadIO m => m AppConfig
loadConfig = liftIO $ Dhall.inputFile Dhall.auto "./config/dev.dhall"

type Config = C.Config

retrieveConfig :: MonadIO m => m C.Config
retrieveConfig = do
  let configPath = "./config/local.conf"
  liftIO $ C.load [C.Required configPath]

getKeysFilePath :: MonadIO m => C.Config -> m FilePath
getKeysFilePath config = liftIO $ C.require config "auth.key_path"

getPort :: MonadIO m => C.Config -> m Int
getPort config = liftIO $ C.require config "web_server.port"

getPoolLimit :: MonadIO m => C.Config -> m Int
getPoolLimit config = liftIO $ C.require config "database.pool_limit"

getLoggerConfig :: MonadIO m => C.Config -> m Log.LoggerConfig
getLoggerConfig config = liftIO $ do
  appInstanceName <- C.require config "log.app_instance_name"
  logToStdout <- C.require config "log.log_to_stdout"
  logLevelRaw <- C.require config "log.log_level"
  pure $
    Log.LoggerConfig
      { appInstanceName = appInstanceName,
        logToStdout = logToStdout,
        logLevel = fromMaybe Log.Debug (readMaybe logLevelRaw)
      }
