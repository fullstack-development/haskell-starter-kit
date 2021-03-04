module Ext.Logger.Config
  ( LoggerConfig (..),
  )
where

import qualified Colog as Log
import qualified Data.Text as T
import System.IO (Handle)

data LoggerConfig = LoggerConfig
  { appInstanceName :: T.Text,
    logToStdout :: Bool,
    logToFile :: Maybe Handle,
    logLevel :: Log.Severity
  }
  deriving (Show, Eq)
