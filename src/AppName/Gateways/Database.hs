module AppName.Gateways.Database
  ( module Exports,
    runAllMigrations,
  )
where

import qualified AppName.Config as C
import AppName.Gateways.Database.Connection as Exports
import AppName.Gateways.Database.Setup as Exports
import AppName.Gateways.Database.Tables.User as Exports
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Postgresql
import Ext.Data.Env (Env (..))

migrateAll :: (MonadIO m) => SqlPersistT m ()
migrateAll = runMigration migrateUser

runAllMigrations :: IO ()
runAllMigrations = do
  conf <- C.retrieveConfig Dev
  withDbPoolDebug conf $ \pool -> liftIO $ runSqlPersistMPool migrateAll pool
