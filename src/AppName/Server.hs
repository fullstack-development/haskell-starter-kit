{-# LANGUAGE DataKinds #-}

module AppName.Server
  ( runDevServer,
  )
where

import AppName.API (API, apiType)
import AppName.AppHandle (AppHandle (..), withAppHandle)
import AppName.Auth (ProtectedServantJWTCtx)
import qualified AppName.Config as C
import AppName.Gateways.Database (withDbPool, withDbPoolDebug)
import AppName.Gateways.Endpoints.FakeLogin (fakeLoginEndpoint)
import AppName.Gateways.Endpoints.GetUsers
  ( getCurrentUserEndpoint,
    getUserByIdEndpoint,
  )
import Control.Exception.Safe (MonadThrow, throw, try)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Pool as Pool
import Data.Proxy (Proxy (Proxy))
import Database.Persist.Sql (SqlBackend)
import Ext.Data.Env (Env (..))
import Ext.Logger.Colog (logTextStdout)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
  ( cors,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
  ( Context (EmptyContext, (:.)),
    Handler (Handler),
    Server,
    ServerT,
    err404,
    hoistServerWithContext,
    serve,
    serveWithContext,
    (:<|>) (..),
  )
import qualified Servant.Auth.Server as SAS

handler ::
  (MonadIO m, MonadThrow m) =>
  SAS.JWTSettings ->
  AppHandle ->
  ServerT API m
handler jwtSettings h = fakeLoginEndpoint jwtSettings :<|> getUserByIdEndpoint h :<|> getCurrentUserEndpoint h

catchServantErrorsFromIO :: ServerT API IO -> Server API
catchServantErrorsFromIO =
  hoistServerWithContext
    apiType
    (Proxy :: ProtectedServantJWTCtx)
    (Handler . ExceptT . try)

runServer :: Env -> C.Config -> IO ()
runServer env config = do
  filePath <- C.getKeysFilePath config
  authKey <- SAS.readKey filePath
  port <- C.getPort config
  let serverSettings =
        setPort port $
          setBeforeMainLoop
            (putStrLn ("listening on port " <> show port))
            defaultSettings
      policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
      jwtSettings = SAS.defaultJWTSettings authKey
      cfg = SAS.defaultCookieSettings :. jwtSettings :. EmptyContext
      server :: MonadIO (m IO) => Settings -> AppHandle -> m IO ()
      server settings ah =
        liftIO
          . runSettings settings
          . cors (const $ Just policy)
          . provideOptions apiType
          . serveWithContext apiType cfg
          . catchServantErrorsFromIO
          . handler jwtSettings
          $ ah
  liftIO $ withAppHandle env $ server serverSettings

runDevServer :: IO ()
runDevServer = do
  config <- C.retrieveConfig Dev
  runServer Dev config
