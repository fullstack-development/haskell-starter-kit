{-# LANGUAGE OverloadedLabels #-}

module Ext.Logger.Config
  ( LoggerConfig (..),
    fromAppConfig,
  )
where

import qualified AppName.Config as C
import qualified Colog as Log
import qualified Data.Text as T

data LoggerConfig = LoggerConfig
  { appInstanceName :: T.Text,
    logToStdout :: Bool,
    logLevel :: Log.Severity
  }
  deriving (Show, Eq)

transformLogLevel :: C.LogLevel -> Log.Severity
transformLogLevel C.Debug = Log.Debug
transformLogLevel C.Info = Log.Info
transformLogLevel C.Warning = Log.Warning
transformLogLevel C.Error = Log.Error

fromAppConfig :: C.LogConfig -> LoggerConfig
fromAppConfig C.LogConfig {..} =
  LoggerConfig
    { appInstanceName = appName,
      logToStdout = logToStdout,
      logLevel = transformLogLevel logLevel
    }
