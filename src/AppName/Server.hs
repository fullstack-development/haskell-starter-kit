{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module AppName.Server
  ( runDevServer,
  )
where

import AppName.API (API, apiType)
import AppName.AppHandle (AppHandle (..), MonadHandler, withAppHandle)
import AppName.Auth (AuthenticatedUser (AuthenticatedClient), ProtectedServantJWTCtx, defaultJWTSettings)
import AppName.Auth.Commands
import qualified AppName.Config as C
import qualified AppName.Domain.PhoneVerification as Phone
import AppName.Gateways.Database (User (User), loadUserByPhone, withDbPool, withDbPoolDebug)
import AppName.Gateways.Endpoints.FakeLogin (fakeLoginEndpoint)
import AppName.Gateways.Endpoints.GetUsers
  ( getCurrentUserEndpoint,
    getOrCreateUserByPhoneEndpoint,
    getUserByIdEndpoint,
  )
import qualified AppName.Gateways.Endpoints.PhoneVerification as Phone
import Control.Exception.Safe (MonadThrow, throw, try)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Pool as Pool
import Data.Proxy (Proxy (Proxy))
import Database.Persist.Sql (Entity (entityKey), SqlBackend, fromSqlKey, runSqlPersistMPool)
import Ext.Logger.Colog (logTextStdout)
import qualified Ext.Logger.Colog as Log
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
  ( (:<|>) (..),
    Context ((:.), EmptyContext),
    Handler (Handler),
    Server,
    ServerT,
    err404,
    hoistServerWithContext,
    serve,
    serveWithContext,
  )
import qualified Servant.Auth.Server as SAS

buildHandlers ::
  forall (mexternal :: * -> *) (minternal :: * -> *).
  (MonadIO mexternal, MonadHandler minternal) =>
  SAS.JWTSettings ->
  AppHandle ->
  mexternal (ServerT API minternal)
buildHandlers jwtSettings h = do
  phoneVerification <- liftIO buildPhoneVerification
  pure $ fakeLoginEndpoint jwtSettings :<|> phoneVerification :<|> getUserByIdEndpoint h :<|> getCurrentUserEndpoint h
  where
    -- env = mkEnv h
    buildPhoneVerification :: IO (ServerT Phone.PhoneAuthAPI minternal)
    buildPhoneVerification =
      Phone.phoneVerificationAPI
        Phone.defParams
        Phone.Externals
          { eLogger = appHandleLogger h,
            eJwtSettings = jwtSettings,
            eRetrieveUserByPhone = getOrCreateUserByPhoneEndpoint h,
            eSendCodeToUser = codePrinter
          }
    codePrinter phone code = print $ "code sent: " <> Phone.codeToText code

catchServantErrorsFromIO :: Log.LogAction IO Log.Message -> ServerT API (Log.LoggerT Log.Message IO) -> Server API
catchServantErrorsFromIO env =
  hoistServerWithContext
    apiType
    (Proxy :: ProtectedServantJWTCtx)
    (Handler . ExceptT . try . Log.usingLoggerT env)

runServer :: C.Config -> IO ()
runServer config = do
  checkAuthKey
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
      server settings ah = do
        handler <- buildHandlers jwtSettings ah
        liftIO
          . runSettings settings
          . cors (const $ Just policy)
          . provideOptions apiType
          . serveWithContext apiType cfg
          . catchServantErrorsFromIO undefined
          $ handler
  liftIO $ withAppHandle $ server serverSettings

runDevServer :: IO ()
runDevServer = do
  config <- C.retrieveConfig
  runServer config
