{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ext.Logger.Colog
  ( LoggerT (..),
    runWithAction,
    mkLogActionIO,
    setLineBuffering,
  )
where

import qualified Colog
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
import qualified Ext.Logger as Log
import qualified Ext.Logger.Config as Conf
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
  )

-- | This is an instance of 'MonadLogger' based on 'Colog'.
newtype LoggerT m a = LoggerT {runLoggerT :: Colog.LoggerT Colog.Message m a}
  deriving (Functor, Applicative, Monad)

instance MonadIO m => Log.MonadLogger (LoggerT m) where
  logMessage = cologLogMessage

instance MonadIO m => MonadIO (LoggerT m) where
  liftIO = LoggerT . liftIO

cologLogMessage :: MonadIO m => Log.Severity -> Log.CallStack -> T.Text -> LoggerT m ()
cologLogMessage severity callSite messageText = LoggerT $ Colog.logMsg cologMsg
  where
    cologMsg =
      Colog.Msg
        { msgSeverity = cologSeverityFromSeverity severity,
          msgText = messageText,
          msgStack = Log.unCallStack callSite
        }

cologSeverityFromSeverity :: Log.Severity -> Colog.Severity
cologSeverityFromSeverity = \case
  Log.Debug -> Colog.Debug
  Log.Info -> Colog.Info
  Log.Warning -> Colog.Warning
  Log.Error -> Colog.Error

runWithAction :: Monad m => Colog.LogAction m Colog.Message -> LoggerT m a -> m a
runWithAction action = Colog.usingLoggerT action . runLoggerT

type instance Colog.FieldType "timestamp" = Time.UTCTime

type instance Colog.FieldType "appInstanceName" = T.Text

fieldMapIO :: MonadIO m => Conf.LoggerConfig -> Colog.FieldMap m
fieldMapIO conf = timestampedFieldMapIO <> fieldMap conf

timestampedFieldMapIO ::
  forall m.
  MonadIO m =>
  Colog.FieldMap m
timestampedFieldMapIO = [#timestamp Clock.now]

fieldMap :: Monad m => Conf.LoggerConfig -> Colog.FieldMap m
fieldMap Conf.LoggerConfig {..} = [#appInstanceName (pure appInstanceName)]

fmtRichMessage :: Monad m => Colog.RichMsg m Colog.Message -> m BS.ByteString
fmtRichMessage Colog.RichMsg {richMsgMsg = Colog.Msg {..}, ..} = do
  timestamp <- Colog.extractField $ TM.lookup @"timestamp" richMsgMap
  appInstanceName <- Colog.extractField $ TM.lookup @"appInstanceName" richMsgMap
  let logObj =
        J.object
          [ "timestamp" .= timestamp,
            "appInstanceName" .= appInstanceName,
            "severity" .= show msgSeverity,
            "trace" .= Colog.showSourceLoc msgStack,
            "message" .= msgText
          ]
  pure $ LBS.toStrict $ J.encode logObj

mkLogActionIO :: MonadIO m => Conf.LoggerConfig -> Colog.LogAction m Colog.Message
mkLogActionIO conf@Conf.LoggerConfig {..} =
  Colog.filterBySeverity (cologSeverityFromSeverity logLevel) Colog.msgSeverity $
    Colog.upgradeMessageAction (fieldMapIO conf) $
      Colog.cmapM fmtRichMessage stdoutLogger
  where
    stdoutLogger =
      if logToStdout
        then Colog.logByteStringStdout
        else mempty

setLineBuffering :: MonadIO m => m ()
setLineBuffering = liftIO $ hSetBuffering stdout LineBuffering
