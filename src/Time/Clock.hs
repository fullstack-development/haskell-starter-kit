module Time.Clock
  ( MonadClock(..)
  , now
  ) where

import qualified Chronos                as C
import           Control.Monad.IO.Class (MonadIO (..))

class (Monad m) =>
      MonadClock m
  where
  getCurrentTime :: m C.Time

instance MonadClock IO where
  getCurrentTime = C.now

now :: MonadIO m => m C.Time
now = liftIO getCurrentTime
