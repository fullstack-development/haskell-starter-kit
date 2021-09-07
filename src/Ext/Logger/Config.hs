module Ext.Logger.Config
  ( LoggerConfig (..),
  )
where

import qualified Data.Text as T
import qualified Ext.Logger as Log

data LoggerConfig = LoggerConfig
  { appInstanceName :: T.Text,
    logToStdout :: Bool,
    logLevel :: Log.Severity
  }
  deriving (Show, Eq)
