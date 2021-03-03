module Lib
  ( someFunc
  ) where

import qualified Colog as Log
import qualified Data.ByteString as BS
import Data.Functor.Contravariant (Contravariant(contramap))
import qualified Logger.Colog as Log
import qualified Logger.Config as Log

someFunc :: IO ()
someFunc =
  Log.usingLoggerT
    (Log.upgradeMessageAction (Log.fieldMapIO conf) $
     Log.cmapM Log.fmtRichMessage Log.logByteStringStdout)
    example

conf :: Log.LoggerConfig
conf =
  Log.LoggerConfig
    { appInstanceName = "MetaApp"
    , logToStdout = True
    , logToFile = Nothing
    , logLevel = Log.Debug
    }

-- type WithLog env msg m = (MonadReader env m, HasLog env msg m)
example :: Log.WithLog env Log.Message m => m ()
example = do
  Log.logDebug "Starting application..."
  Log.logInfo "Finishing application..."
