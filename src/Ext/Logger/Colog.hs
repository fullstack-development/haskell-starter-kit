{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ext.Logger.Colog
  ( module Export,
    fieldMapIO,
    fieldMapM,
    fmtRichMessage,
    mkLogActionIO,
    logFlush,
    setLineBuffering,
  )
where

import Colog as Export
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.TypeRepMap as TM
import qualified Ext.Data.Time as Clock
import qualified Ext.Data.Time as Time
import qualified Ext.Logger.Config as Conf
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    hFlush,
    hSetBuffering,
    stdout,
  )

type instance FieldType "timestamp" = Time.UTCTime

type instance FieldType "appInstanceName" = T.Text

fieldMapM :: Clock.MonadClock m => Conf.LoggerConfig -> FieldMap m
fieldMapM conf = timestampedFieldMapM <> fieldMap conf

fieldMapIO :: MonadIO m => Conf.LoggerConfig -> FieldMap m
fieldMapIO conf = timestampedFieldMapIO <> fieldMap conf

timestampedFieldMapM ::
  forall m.
  Clock.MonadClock m =>
  FieldMap m
timestampedFieldMapM = [#timestamp Clock.getCurrentTime]

timestampedFieldMapIO ::
  forall m.
  MonadIO m =>
  FieldMap m
timestampedFieldMapIO = [#timestamp Clock.now]

fieldMap :: Monad m => Conf.LoggerConfig -> FieldMap m
fieldMap Conf.LoggerConfig {..} = [#appInstanceName (pure appInstanceName)]

fmtRichMessage :: Monad m => RichMsg m Message -> m BS.ByteString
fmtRichMessage RichMsg {richMsgMsg = Msg {..}, ..} = do
  timestamp <- extractField $ TM.lookup @"timestamp" richMsgMap
  appInstanceName <- extractField $ TM.lookup @"appInstanceName" richMsgMap
  let logObj =
        J.object
          [ "timestamp" .= timestamp,
            "appInstanceName" .= appInstanceName,
            "severity" .= show msgSeverity,
            "trace" .= showSourceLoc msgStack,
            "message" .= msgText
          ]
  pure $ LBS.toStrict $ J.encode logObj

mkLogActionIO :: MonadIO m => Conf.LoggerConfig -> LogAction m Message
mkLogActionIO conf@Conf.LoggerConfig {..} =
  filterBySeverity logLevel msgSeverity
    $ upgradeMessageAction (fieldMapIO conf)
    $ cmapM fmtRichMessage stdoutLogger
  where
    stdoutLogger =
      if logToStdout
        then logByteStringStdout
        else mempty

logFlush :: MonadIO m => Handle -> LogAction m a
logFlush handle = LogAction $ const $ liftIO $ hFlush handle

setLineBuffering :: MonadIO m => m ()
setLineBuffering = liftIO $ hSetBuffering stdout LineBuffering
