{-# LANGUAGE ConstraintKinds #-}

-- The application logger interface module. This should be minimal
-- possible and independent of a particular logging library or
-- implementation.
--
-- The module is intended to be imported qualified with an alias like
-- @Log@.
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

-- | You should generally prefer using this type as a constraint
-- instead of 'MonadLogger' to enable logging. This is necessary to
-- capture the caller function name within the GHC call stack.
type WithLog m = (GHC.HasCallStack, MonadLogger m)

class Monad m => MonadLogger m where
  logMessage :: Severity -> CallStack -> T.Text -> m ()

data Severity = Debug | Info | Warning | Error
  deriving (Eq, Show, Read)

newtype CallStack = CallStack {unCallStack :: GHC.CallStack}

-- Yoy may use them as `Log.info`, `Log.error` etc. if you imported
-- the module qualified with the corresponding alias.
debug, info, warn, error :: (GHC.HasCallStack, MonadLogger m) => T.Text -> m ()
debug = GHC.withFrozenCallStack $ logCapturingCallStack Debug
info = GHC.withFrozenCallStack $ logCapturingCallStack Info
warn = GHC.withFrozenCallStack $ logCapturingCallStack Warning
error = GHC.withFrozenCallStack $ logCapturingCallStack Error

logCapturingCallStack :: (GHC.HasCallStack, MonadLogger m) => Severity -> T.Text -> m ()
logCapturingCallStack severity = GHC.withFrozenCallStack (logMessage severity $ CallStack GHC.callStack)
