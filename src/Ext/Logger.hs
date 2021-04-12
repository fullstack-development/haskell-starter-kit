{-# LANGUAGE ConstraintKinds #-}

-- The application logger interface module. This should be minimal
-- possible and independent of a particular logging library or
-- implementation.
module Ext.Logger
  ( WithLog,
    MonadLogger (..),
    Severity (..),
    CallStack (..),
    debug,
    info,
    warn,
    error,
  )
where

import qualified Data.Text as T
import qualified GHC.Stack as GHC
import Prelude hiding (error)

-- | Use the type instead of 'MonadLogger' to pass the logger monad
-- constraint. This is necessary to capture the caller function name
-- within the GHC call stack.
type WithLog m = (GHC.HasCallStack, MonadLogger m)

class Monad m => MonadLogger m where
  logMessage :: Severity -> CallStack -> T.Text -> m ()

data Severity = Debug | Info | Warning | Error
  deriving (Eq, Show, Read)

newtype CallStack = CallStack {unCallStack :: GHC.CallStack}

debug, info, warn, error :: (GHC.HasCallStack, MonadLogger m) => T.Text -> m ()
debug = GHC.withFrozenCallStack $ logCapturingCallStack Debug
info = GHC.withFrozenCallStack $ logCapturingCallStack Info
warn = GHC.withFrozenCallStack $logCapturingCallStack Warning
error = GHC.withFrozenCallStack $ logCapturingCallStack Error

logCapturingCallStack :: (GHC.HasCallStack, MonadLogger m) => Severity -> T.Text -> m ()
logCapturingCallStack severity = GHC.withFrozenCallStack (logMessage severity $ CallStack GHC.callStack)
