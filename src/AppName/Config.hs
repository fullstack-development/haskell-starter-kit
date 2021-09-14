{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}

module AppName.Config where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Dhall
import Ext.Logger.Config (LoggerConfig (..))
import GHC.Generics (Generic)

data AppConfig = AppConfig
  { authConfig :: AuthConfig,
    loggerConfig :: LoggerConfig,
    dbConfig :: DbConfig,
    appPort :: Int
  }
  deriving (Generic, Dhall.FromDhall, Show)

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

loadConfig :: MonadIO m => FilePath -> m AppConfig
loadConfig = liftIO . Dhall.inputFile Dhall.auto
