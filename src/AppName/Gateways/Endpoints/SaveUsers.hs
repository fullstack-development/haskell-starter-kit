{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.SaveUsers
  ( saveUserPersonalInfoEndpoint,
  )
where

import AppName.API.User (AddressSerializer (..), PersonalInfoSerializer (..), UserSerializer (..))
import AppName.AppHandle (AppHandle (..))
import AppName.Auth (AuthenticatedUser (AuthenticatedClient))
import qualified AppName.Domain.PhoneVerification as Phone
import AppName.Gateways.Database
  ( Key (UserKey),
    User (..),
    saveUserPersonalInfo,
  )
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Data.Functor (($>))
import Database.Persist.Postgresql
import qualified Database.Persist.Postgresql as P
import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web
import Servant (err404)
import qualified Servant.Auth.Server as SAS

saveUserPersonalInfoEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> SAS.AuthResult AuthenticatedUser -> PersonalInfoSerializer -> m (Web.WebApiHttpResponse ())
saveUserPersonalInfoEndpoint AppHandle {..} (SAS.Authenticated (AuthenticatedClient userId)) personalInfoSerializer =
  liftIO . flip runSqlPersistMPool appHandleDbPool $
    saveUserPersonalInfo sqlUserId personalInfoSerializer $> Web.success
  where
    sqlUserId = toSqlKey $ fromIntegral userId
