{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}

module AppName.Config
  ( Config,
    C.require,
    C.lookup,
    retrieveConfig,
    getKeysFilePath,
    getPort,
    getPoolLimit,
    AppConfig (..),
    DbConfig (..),
    AuthConfig (..),
    LogLevel (..),
    LogToFile (..),
    LogConfig (..),
    loadConfig,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Text (Text)
import qualified Dhall
import GHC.Generics (Generic)

data DbConfig = DbConfig
  { host :: Text,
    port :: Int,
    database :: Text,
    user :: Text,
    password :: Text,
    poolLimit :: Int
  }
  deriving (Generic, Dhall.FromDhall, Show)

newtype AuthConfig = AuthConfig {pathToKey :: Text}
  deriving (Generic, Dhall.FromDhall, Show)

data LogLevel = Debug | Info | Warning | Error
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
    dbConfig :: DbConfig,
    appPort :: Int
  }
  deriving (Generic, Dhall.FromDhall, Show)

loadConfig :: MonadIO m => FilePath -> m AppConfig
loadConfig = liftIO . Dhall.inputFile Dhall.auto

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
