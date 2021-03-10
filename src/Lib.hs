module Lib
  ( runDefaultExample,
    runMigrationsAndServer,
  )
where

import qualified AppName.Config as C
<<<<<<< HEAD
import AppName.Domain.PhoneVerification (UncheckedPhone (UncheckedPhone), checkPhone)
=======
import AppName.Gateways.Database (runAllMigrations)
>>>>>>> origin/master
import AppName.Gateways.Database.Setup (withDbPool, withDbPoolDebug)
import AppName.Gateways.Database.Tables.User (createUserRecord, loadUserById)
import AppName.Server (runDevServer)
import qualified Colog as Log
import Control.Monad.IO.Unlift (liftIO)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Functor.Contravariant (Contravariant (contramap))
import Database.Persist.Postgresql
import qualified Ext.Logger.Colog as Log
import qualified Ext.Logger.Config as Log

runDefaultExample :: IO ()
runDefaultExample =
  Log.usingLoggerT (Log.mkLogActionIO logConf) $ do
    liftIO runAllMigrations
    config <- liftIO C.retrieveConfig
    runLogExample
    runDBExample config
    liftIO runDevServer

runMigrationsAndServer :: IO ()
runMigrationsAndServer =
  Log.usingLoggerT (Log.mkLogActionIO logConf) $ do
    runLogExample
    liftIO runAllMigrations
    Log.logDebug "starting server"
    liftIO runDevServer

logConf :: Log.LoggerConfig
logConf =
  Log.LoggerConfig
    { appInstanceName = "AppName",
      logToStdout = True,
      logLevel = Log.Debug
    }

runDBExample config =
  liftIO
    . withDbPoolDebug config
    $ \pool ->
      dbExample pool
  where
    dbExample pool = liftIO . flip runSqlPersistMPool pool $ do
      phone <- either (error "Phone is incorrect") pure . checkPhone $ UncheckedPhone "+79990424242"
      (usedId, _) <- createUserRecord phone
      user <- loadUserById usedId
      liftIO $ print user
      pure ()

-- type WithLog env msg m = (MonadReader env m, HasLog env msg m)
runLogExample :: Log.WithLog env Log.Message m => m ()
runLogExample = do
  Log.logInfo "Starting application..."
  Log.logDebug "Here is how we work!"
