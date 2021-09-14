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
import qualified AppName.Gateways.CryptoRandomGen as CryptoRandomGen
import AppName.Gateways.Database (withDbPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import qualified Ext.Logger as Log
import Ext.Logger.Config (LoggerConfig)

data AppHandle = AppHandle
  { appHandleDbPool :: Pool SqlBackend,
    appHandleAppConfig :: C.AppConfig,
    appHandleLogger :: LoggerConfig,
    appHandleRandomGen :: CryptoRandomGen.Ref
  }

type MonadHandler m = (MonadIO m, Log.WithLog m)

withAppHandle :: (AppHandle -> NoLoggingT IO b) -> IO b
withAppHandle action = do
  appConfig <- C.loadConfig "./config/dev.dhall"
  let loggerConfig = C.loggerConfig appConfig
  randomGen <- CryptoRandomGen.newRef
  liftIO . withDbPool appConfig $ \pool ->
    action $
      AppHandle
        { appHandleDbPool = pool,
          appHandleAppConfig = appConfig,
          appHandleLogger = loggerConfig,
          appHandleRandomGen = randomGen
        }
