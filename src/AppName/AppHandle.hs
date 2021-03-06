{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module AppName.AppHandle
  ( AppHandle (..),
    withAppHandle,
    MonadHandler,
  )
where

import qualified AppName.Config as C
import AppName.Gateways.Database (withDbPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import qualified Ext.Logger.Colog as Log
import Ext.Logger.Config (LoggerConfig)

data AppHandle
  = AppHandle
      { appHandleDbPool :: Pool SqlBackend,
        appHandleConfig :: C.Config,
        appHandleLogger :: LoggerConfig
      }

type MonadHandler m = (MonadIO m, Log.WithLog (Log.LogAction m Log.Message) Log.Message m)

withAppHandle :: (AppHandle -> NoLoggingT IO b) -> IO b
withAppHandle action = do
  config <- C.retrieveConfig
  loggerConfig <- C.getLoggerConfig config
  liftIO . withDbPool config $ \pool ->
    action $ AppHandle pool config loggerConfig
