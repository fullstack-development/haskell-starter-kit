module Lib
  ( someFunc
  ) where

import qualified Colog as Log
import Data.Functor.Contravariant (Contravariant(contramap))

someFunc :: IO ()
someFunc = Log.usingLoggerT (contramap Log.fmtMessage Log.logTextStdout) example

example :: Log.WithLog env Log.Message m => m ()
example = do
  Log.logDebug "Starting application..."
  Log.logInfo "Finishing application..."
