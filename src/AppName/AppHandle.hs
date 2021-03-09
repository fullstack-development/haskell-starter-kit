{-# LANGUAGE RankNTypes #-}

module AppName.AppHandle
  ( AppHandle (..),
    withAppHandle,
  )
where

import qualified AppName.Config as C
import AppName.Gateways.Database (withDbPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql (SqlBackend)
import Ext.Logger.Config (LoggerConfig)

data AppHandle
  = AppHandle
      { appHandleDbPool :: Pool SqlBackend,
        appHandleConfig :: C.Config,
        appHandleLogger :: LoggerConfig
      }

withAppHandle :: (AppHandle -> NoLoggingT IO b) -> IO b
withAppHandle action = do
  config <- C.retrieveConfig
  loggerConfig <- C.getLoggerConfig config
  liftIO . withDbPool config $ \pool ->
    action $ AppHandle pool config loggerConfig