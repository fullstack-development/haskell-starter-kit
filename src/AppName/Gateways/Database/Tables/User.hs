{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AppName.Gateways.Database.Tables.User where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
  ( Entity,
    PersistValue (PersistInt64),
    SqlPersistT,
    entityKey,
    entityVal,
    from,
    fromSqlKey,
    insert,
    rawSql,
    select,
    toSqlKey,
    val,
    where_,
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
    deriving Show
|]

createUserRecord ::
  (MonadUnliftIO m) =>
  T.Text ->
  SqlPersistT m (P.Key User, Time.UTCTime)
createUserRecord phone = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      User
        now
        phone
  pure (rowOrderId, now)

loadUserById ::
  MonadUnliftIO m =>
  P.Key User ->
  SqlPersistT m (Maybe (P.Entity User))
loadUserById userId =
  fmap listToMaybe . select $
    from $ \user -> do
      where_ $ user ^. UserId ==. val userId
      pure user
