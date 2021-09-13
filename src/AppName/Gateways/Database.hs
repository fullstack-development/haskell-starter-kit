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
import Database.Persist.Postgresql (Migration, SqlPersistT, runMigration, runSqlPersistMPool)

migrateAll :: (MonadIO m) => SqlPersistT m ()
migrateAll = mapM_ runMigration allMigrations

allMigrations :: [Migration]
allMigrations =
  [migrateUser]

runAllMigrations :: IO ()
runAllMigrations = do
  conf <- C.loadConfig "./config/dev.dhall"
  withDbPoolDebug conf $ liftIO . runSqlPersistMPool migrateAll
