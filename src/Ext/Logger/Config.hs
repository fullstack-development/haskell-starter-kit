{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}

module Ext.Logger.Config where

import qualified Colog
import Data.Text (Text)
import qualified Dhall
import GHC.Generics (Generic)

data Severity = Debug | Info | Warning | Error
  deriving (Eq, Show, Read, Generic, Dhall.FromDhall)

data LogToFile = NoLogToFile | AllowLogToFile Text
  deriving (Generic, Dhall.FromDhall, Show)

data LoggerConfig = LoggerConfig
  { appName :: Text,
    logToStdout :: Bool,
    logLevel :: Severity,
    logRawSql :: Bool,
    logToFile :: LogToFile
  }
  deriving (Generic, Dhall.FromDhall, Show)

transformLogLevel :: Severity -> Colog.Severity
transformLogLevel Debug = Colog.Debug
transformLogLevel Info = Colog.Info
transformLogLevel Warning = Colog.Warning
transformLogLevel Error = Colog.Error
