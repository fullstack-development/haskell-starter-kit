{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.GetUsers
  ( getUserByIdEndpoint,
    getCurrentUserEndpoint,
    getOrCreateUserByPhoneEndpoint,
  )
where

import AppName.API.User (AddressSerializer (..), UserSerializer (..))
import AppName.AppHandle (AppHandle (..), MonadHandler)
import AppName.Auth (AuthenticatedUser (..))
import qualified AppName.Domain.PhoneVerification as Phone
import AppName.Gateways.Database
  ( Key (UserKey),
    User (..),
    createUserRecord,
    loadUserById,
    loadUserByPhone,
  )
import Control.Exception.Safe (throw)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Database.Persist.Postgresql
import qualified Ext.Logger as Log
import Servant (err401)
import qualified Servant.Auth.Server as SAS

getUserByIdEndpoint ::
  (MonadHandler m) => AppHandle -> Int -> m (Maybe UserSerializer)
getUserByIdEndpoint AppHandle {..} userId =
  -- !!!TODO check that current user is Admin
  fmap (fmap mapUser) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
       in UserSerializer
            { usId = fromIntegral . fromSqlKey $ entityKey entity,
              usCreatedAt = userCreatedAt,
              usPhone = userPhone,
              usDateOfBirth = userDateOfBirth,
              usAddress =
                Just
                  AddressSerializer
                    { asStreet = userAddressStreet,
                      asCity = userAddressCity,
                      asZipCode = userAddressZipCode
                    }
            }

getOrCreateUserByPhoneEndpoint ::
  (MonadHandler m) => AppHandle -> Phone.Phone -> m AuthenticatedUser
getOrCreateUserByPhoneEndpoint AppHandle {..} phoneNumber =
  fmap (AuthenticatedClient . fromIntegral . fromSqlKey) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      do
        user <- loadUserByPhone phoneNumber
        maybe createNewUser (pure . entityKey) user
  where
    createNewUser =
      fst <$> createUserRecord phoneNumber

getCurrentUserEndpoint ::
  (MonadHandler m) => AppHandle -> SAS.AuthResult AuthenticatedUser -> m (Maybe UserSerializer)
getCurrentUserEndpoint AppHandle {..} (SAS.Authenticated (AuthenticatedClient userId)) =
  fmap (fmap mapUser) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
       in UserSerializer
            { usId = fromIntegral . fromSqlKey $ entityKey entity,
              usCreatedAt = userCreatedAt,
              usPhone = userPhone,
              usDateOfBirth = userDateOfBirth,
              usAddress =
                Just
                  AddressSerializer
                    { asStreet = userAddressStreet,
                      asCity = userAddressCity,
                      asZipCode = userAddressZipCode
                    }
            }
getCurrentUserEndpoint _ _ = do
  Log.logError "getCurrentUserEndpoint: Unauthorized access"
  liftIO $ throw err401
