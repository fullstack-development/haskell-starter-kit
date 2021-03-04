module Ext.Data.Time
  ( MonadClock (..),
    now,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Time as Time

class
  (Monad m) =>
  MonadClock m
  where
  getCurrentTime :: m Time.UTCTime

instance MonadClock IO where
  getCurrentTime = Time.getCurrentTime

now :: MonadIO m => m Time.UTCTime
now = liftIO getCurrentTime
