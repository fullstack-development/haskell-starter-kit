module Logger.Config
  ( LoggerConfig(..)
  ) where

import qualified Colog as Log
import qualified Data.Text as T

data LoggerConfig = LoggerConfig
  { appInstanceName :: T.Text
  , logToStdout :: Bool
  , logToFile :: Maybe FilePath
  , logLevel :: Log.Severity
  } deriving (Show, Eq)
