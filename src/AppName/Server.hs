{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module AppName.Server
  ( runDevServer,
  )
where

import AppName.API (API, apiType)
import AppName.AppHandle (AppHandle (..), MonadHandler, withAppHandle)
import AppName.Auth (ProtectedServantJWTCtx)
import AppName.Auth.Commands
import qualified AppName.Config as C
import qualified AppName.Domain.PhoneVerification as Phone
import AppName.Gateways.Endpoints.FakeLogin (fakeLoginEndpoint)
import AppName.Gateways.Endpoints.GetUsers
  ( getCurrentUserEndpoint,
    getOrCreateUserByPhoneEndpoint,
    getUserByIdEndpoint,
  )
import qualified AppName.Gateways.Endpoints.PhoneVerification as Phone
import AppName.Gateways.Endpoints.SaveUsers (saveUserPersonalInfoEndpoint)
import qualified Colog
import Control.Exception.Safe (try)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import qualified Ext.Logger.Colog as CologAdapter
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setPort,
  )
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
    hoistServerWithContext,
    serveWithContext,
    (:<|>) (..),
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
  pure $ fakeLoginEndpoint jwtSettings :<|> phoneVerification :<|> getUsers :<|> saveUserPersonalInfoEndpoint h
  where
    getUsers = getUserByIdEndpoint h :<|> getCurrentUserEndpoint h
    buildPhoneVerification :: IO (ServerT Phone.PhoneAuthAPI minternal)
    buildPhoneVerification =
      Phone.phoneVerificationAPI
        Phone.defParams
        Phone.Externals
          { eJwtSettings = jwtSettings,
            eRetrieveUserByPhone = getOrCreateUserByPhoneEndpoint h,
            eSendCodeToUser = codePrinter,
            eRandomGen = appHandleRandomGen h
          }
    codePrinter _phone code = print $ "code sent: " <> Phone.codeToText code

hoistServerHandler :: Colog.LogAction IO Colog.Message -> ServerT API (CologAdapter.LoggerT IO) -> Server API
hoistServerHandler env =
  hoistServerWithContext
    apiType
    (Proxy :: ProtectedServantJWTCtx)
    (Handler . ExceptT . try . CologAdapter.runWithAction env)

runServer :: C.AppConfig -> IO ()
runServer config = do
  checkAuthKey
  let filePath = unpack $ C.pathToKey $ C.authConfig config
  authKey <- SAS.readKey filePath
  let port = C.appPort config
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
          . hoistServerHandler (CologAdapter.mkLogActionIO (appHandleLogger ah))
          $ handler
  liftIO $ withAppHandle $ server serverSettings

runDevServer :: IO ()
runDevServer = do
  config <- C.loadConfig "./config/dev.dhall"
  runServer config
