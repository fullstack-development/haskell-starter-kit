{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Logger.Colog
  ( module Export
  , fieldMapIO
  , fieldMapM
  ) where

import qualified Chronos as C
import Colog as Export
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified Logger.Config as Conf
import qualified Time.Clock as Time

type instance FieldType "timestamp" = C.Time

type instance FieldType "appInstanceName" = T.Text

fieldMapM :: Time.MonadClock m => Conf.LoggerConfig -> FieldMap m
fieldMapM conf = timestampedFieldMapM <> fieldMap conf

fieldMapIO :: MonadIO m => Conf.LoggerConfig -> FieldMap m
fieldMapIO conf = timestampedFieldMapIO <> fieldMap conf

timestampedFieldMapM ::
     forall m. Time.MonadClock m
  => FieldMap m
timestampedFieldMapM = [#timestamp Time.getCurrentTime]

timestampedFieldMapIO ::
     forall m. MonadIO m
  => FieldMap m
timestampedFieldMapIO = [#timestamp Time.now]

fieldMap :: Monad m => Conf.LoggerConfig -> FieldMap m
fieldMap Conf.LoggerConfig {..} = [#appInstanceName (pure appInstanceName)]
