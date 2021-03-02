module Lib
  ( someFunc
  ) where

import qualified Colog as Log
import Data.Functor.Contravariant (Contravariant(contramap))
import qualified Data.ByteString as BS

someFunc :: IO ()
someFunc = Log.usingLoggerT (Log.upgradeMessageAction Log.defaultFieldMap $ Log.cmapM Log.fmtRichMessageDefault Log.logTextStdout) example

fmt :: Log.RichMsg m Log.Message -> m BS.ByteString
fmt = error "not implemented"

-- type WithLog env msg m = (MonadReader env m, HasLog env msg m)
example :: Log.WithLog env Log.Message m => m ()
example = do
  Log.logDebug "Starting application..."
  Log.logInfo "Finishing application..."
