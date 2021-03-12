{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.SaveUsers
  ( saveUserPersonalInfoEndpoint,
  )
where

import AppName.API.User (PersonalInfoSerializer (..))
import AppName.AppHandle (AppHandle (..), MonadHandler)
import AppName.Auth (AuthenticatedUser (AuthenticatedClient))
import AppName.Gateways.Database (saveUserPersonalInfo)
import Control.Exception.Safe (throw)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Data.Functor (($>))
import Database.Persist.Postgresql (runSqlPersistMPool, toSqlKey)
import qualified Ext.HTTP.Response as Web
import qualified Ext.Logger.Colog as Log
import Servant (err401)
import qualified Servant.Auth.Server as SAS

saveUserPersonalInfoEndpoint ::
  (MonadHandler m) => AppHandle -> SAS.AuthResult AuthenticatedUser -> PersonalInfoSerializer -> m (Web.WebApiHttpResponse ())
saveUserPersonalInfoEndpoint AppHandle {..} (SAS.Authenticated (AuthenticatedClient userId)) personalInfoSerializer =
  liftIO . flip runSqlPersistMPool appHandleDbPool $
    saveUserPersonalInfo sqlUserId personalInfoSerializer $> Web.success
  where
    sqlUserId = toSqlKey $ fromIntegral userId
saveUserPersonalInfoEndpoint _ _ _ = do
  Log.logError "saveUserPersonalInfoEndpoint: Unauthorized access"
  liftIO $ throw err401
