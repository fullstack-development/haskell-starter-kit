{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.GetUsers
  ( getUserByIdEndpoint,
  )
where

import AppName.AppHandle (AppHandle (..))
import AppName.Gateways.Database
  ( Key (UserKey),
    User (..),
    loadUserById,
  )
import AppName.Serializers.User (UserSerializer (..))
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Postgresql
import qualified Database.Persist.Postgresql as P
import Servant (err404)

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
