module Time.Clock
  ( MonadClock(..)
  , now
  ) where

import qualified Data.Time                as Time
import           Control.Monad.IO.Class (MonadIO (..))

class (Monad m) =>
      MonadClock m
  where
  getCurrentTime :: m Time.UTCTime

instance MonadClock IO where
  getCurrentTime = Time.getCurrentTime

now :: MonadIO m => m Time.UTCTime
now = liftIO getCurrentTime
