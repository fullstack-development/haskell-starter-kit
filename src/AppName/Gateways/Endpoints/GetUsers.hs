{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.GetUsers
  ( getUserByIdEndpoint,
    getCurrentUserEndpoint,
    getOrCreateUserByPhoneEndpoint,
  )
where

import AppName.API.User (UserSerializer (..))
import AppName.AppHandle (AppHandle (..))
import AppName.AppHandle (MonadHandler)
import AppName.Auth (AuthenticatedUser (AuthenticatedClient))
import qualified AppName.Domain.PhoneVerification as Phone
import AppName.Gateways.Database
  ( Key (UserKey),
    User (..),
    createUserRecord,
    loadUserById,
    loadUserByPhone,
  )
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Database.Persist.Postgresql
import qualified Ext.Logger.Colog as Log
import qualified Servant.Auth.Server as SAS

getUserByIdEndpoint ::
  (MonadHandler m) => AppHandle -> Int -> m (Maybe UserSerializer)
getUserByIdEndpoint AppHandle {..} userId =
  fmap (fmap mapUser)
    $ liftIO . flip runSqlPersistMPool appHandleDbPool
    $ loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
       in UserSerializer
            { userId = fromIntegral . fromSqlKey $ entityKey entity,
              userCreatedAt = userCreatedAt,
              userPhone = userPhone
            }

getOrCreateUserByPhoneEndpoint ::
  (MonadHandler m) => AppHandle -> Phone.Phone -> m AuthenticatedUser
getOrCreateUserByPhoneEndpoint AppHandle {..} phoneNumber =
  fmap (AuthenticatedClient . fromIntegral . fromSqlKey)
    $ liftIO . flip runSqlPersistMPool appHandleDbPool
    $ do
      user <- loadUserByPhone phoneNumber
      maybe createNewUser (pure . entityKey) user
  where
    createNewUser =
      fst <$> createUserRecord phoneNumber

getCurrentUserEndpoint ::
  (MonadHandler m) => AppHandle -> SAS.AuthResult AuthenticatedUser -> m (Maybe UserSerializer)
getCurrentUserEndpoint AppHandle {..} (SAS.Authenticated (AuthenticatedClient userId)) =
  fmap (fmap mapUser)
    $ liftIO . flip runSqlPersistMPool appHandleDbPool
    $ loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
       in UserSerializer
            { userId = fromIntegral . fromSqlKey $ entityKey entity,
              userCreatedAt = userCreatedAt,
              userPhone = userPhone
            }
getCurrentUserEndpoint _ _ = do
  Log.logError "getCurrentUserEndpoint: Unauthorized access"
  pure Nothing
