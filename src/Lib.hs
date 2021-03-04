module Lib
  ( runDefaultExample,
  )
where

import AppName.Gateways.Database.Setup (withDbPool, withDbPoolDebug)
import AppName.Gateways.Database.Tables.User (createUserRecord)
import qualified Colog as Log
import qualified Config as C
import Control.Monad.IO.Unlift (liftIO)
import qualified Data.ByteString as BS
import Data.Functor.Contravariant (Contravariant (contramap))
import Database.Persist.Postgresql
import Ext.Data.Env (Env (..))
import qualified Logger.Colog as Log
import qualified Logger.Config as Log

runDefaultExample :: IO ()
runDefaultExample = do
  config <- C.retrieveConfig Dev

  Log.usingLoggerT (Log.mkLogActionIO logConf) $ runDBExample config Dev

logConf :: Log.LoggerConfig
logConf =
  Log.LoggerConfig
    { appInstanceName = "MetaApp",
      logToStdout = True,
      logToFile = Nothing,
      logLevel = Log.Debug
    }

runDBExample config env =
  case env of
    Prod -> liftIO . withDbPool config $ \pool -> dbExample pool
    _ ->
      liftIO
        . withDbPoolDebug config
        $ \pool ->
          dbExample pool
  where
    dbExample pool = liftIO . flip runSqlPersistMPool pool $ do
      createUserRecord "+7111"
      pure ()

-- type WithLog env msg m = (MonadReader env m, HasLog env msg m)
example :: Log.WithLog env Log.Message m => m ()
example = do
  Log.logDebug "Starting application..."
  Log.logInfo "Finishing application..."
