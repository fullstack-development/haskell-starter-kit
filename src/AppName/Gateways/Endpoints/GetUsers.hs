{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.GetUsers
  ( getUserByIdEndpoint,
    getCurrentUserEndpoint,
  )
where

import AppName.AppHandle (AppHandle (..))
import AppName.Auth (AuthenticatedUser (AuthenticatedClient))
import AppName.Gateways.Database
  ( Key (UserKey),
    User (..),
    loadUserById,
  )
import AppName.Serializers.User (UserSerializer (..))
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Database.Persist.Postgresql
import qualified Database.Persist.Postgresql as P
import Servant (err404)
import qualified Servant.Auth.Server as SAS

getUserByIdEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe UserSerializer)
getUserByIdEndpoint AppHandle {..} userId =
  fmap (fmap mapUser) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
       in UserSerializer
            { userId = fromIntegral . fromSqlKey $ entityKey entity,
              userCreatedAt = userCreatedAt,
              userPhone = userPhone
            }

getCurrentUserEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> SAS.AuthResult AuthenticatedUser -> m (Maybe UserSerializer)
getCurrentUserEndpoint AppHandle {..} (SAS.Authenticated (AuthenticatedClient userId)) =
  fmap (fmap mapUser) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
       in UserSerializer
            { userId = fromIntegral . fromSqlKey $ entityKey entity,
              userCreatedAt = userCreatedAt,
              userPhone = userPhone
            }
