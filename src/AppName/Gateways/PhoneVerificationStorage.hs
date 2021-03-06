module AppName.Gateways.PhoneVerificationStorage where

import AppName.Domain.PhoneVerification (Phone, WaitConfirmationEntry)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVarIO,
  )
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as M

type Key = Phone

type Val = WaitConfirmationEntry

newtype MemoryStorage = MemoryStorage (TVar (M.Map Key Val))

class Storage s where
  mkStorage :: MonadIO m => m s
  setToStorage :: (MonadIO m) => Key -> Val -> s -> m ()
  getFromStorage :: (MonadIO m) => Key -> s -> m (Maybe Val)
  removeFromStorage :: MonadIO m => Key -> s -> m ()

instance Storage MemoryStorage where
  mkStorage = MemoryStorage <$> liftIO (newTVarIO mempty)
  setToStorage k v (MemoryStorage var) =
    liftIO . atomically . modifyTVar var . M.insert k $ v
  getFromStorage k (MemoryStorage var) = do
    (M.!? k) <$> liftIO (readTVarIO var)
  removeFromStorage k (MemoryStorage var) =
    liftIO . atomically . modifyTVar var . M.delete $ k
