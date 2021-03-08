{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AppName.Gateways.Database.Tables.User where

import AppName.API.User (AddressSerializer (..), PersonalInfoSerializer (..))
import AppName.Domain.PhoneVerification (Phone, phoneToText)
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
  ( Entity,
    PersistValue (PersistInt64),
    SqlPersistT,
    entityKey,
    entityVal,
    from,
    insert,
    rawSql,
    select,
    set,
    toSqlKey,
    update,
    val,
    where_,
    (=.),
    (==.),
    (^.),
  )
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
import Database.PostgreSQL.Simple (SqlError (..))
import Ext.Data.Time (now)
import UnliftIO.Exception (catch)

share
  [mkPersist sqlSettings, mkMigrate "migrateUser"]
  [persistLowerCase|
User
    createdAt Time.UTCTime
    phone T.Text
    dateOfBirth Time.UTCTime Maybe
    addressStreet T.Text Maybe
    addressCity T.Text Maybe
    addressZipCode T.Text Maybe
    deriving Show
|]

createUserRecord ::
  (MonadUnliftIO m) =>
  Phone ->
  SqlPersistT m (P.Key User, Time.UTCTime)
createUserRecord phone = do
  now <- liftIO Time.getCurrentTime
  rowId <-
    insert $ User now (phoneToText phone) Nothing Nothing Nothing Nothing
  pure (rowId, now)

saveUserPersonalInfo ::
  (MonadUnliftIO m, MonadThrow m) =>
  P.Key User ->
  PersonalInfoSerializer ->
  SqlPersistT m ()
saveUserPersonalInfo userId PersonalInfoSerializer {..} = do
  update $ \u -> do
    set u [UserDateOfBirth =. val pisDateOfBirth]
    set u [UserAddressStreet =. val asStreet]
    set u [UserAddressCity =. val asCity]
    set u [UserAddressZipCode =. val asZipCode]
    where_ $ u ^. UserId ==. val userId
  where
    AddressSerializer {..} = pisAddress

loadUserById ::
  MonadUnliftIO m =>
  P.Key User ->
  SqlPersistT m (Maybe (P.Entity User))
loadUserById userId =
  fmap listToMaybe . select $
    from $ \user -> do
      where_ $ user ^. UserId ==. val userId
      pure user

loadUserByPhone ::
  MonadUnliftIO m =>
  Phone ->
  SqlPersistT m (Maybe (P.Entity User))
loadUserByPhone phoneNumber =
  fmap listToMaybe . select $
    from $ \user -> do
      where_ $ user ^. UserPhone ==. val (phoneToText phoneNumber)
      pure user
