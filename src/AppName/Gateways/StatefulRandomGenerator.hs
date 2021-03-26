{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AppName.Gateways.StatefulRandomGenerator (StatefulRandomGen (..), AtomicGen, newAtomicGen) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Tuple (swap)
import System.Random (RandomGen)

-- | It mimics System.Random.Stateful.RandomGenM which is missing in
-- our "random" library version.
class RandomGen gen => StatefulRandomGen gen state monad | state -> gen where
  -- | Performs the given pure computation over a RandomGen, managing
  -- the generator state within a monad
  withRandomState :: (gen -> (a, gen)) -> state -> monad a

-- | The atomic, thread-safe, IORef-based RandomGen state.
newtype AtomicGen gen = AtomicGen (IORef gen)

newAtomicGen :: RandomGen gen => gen -> IO (AtomicGen gen)
newAtomicGen = fmap AtomicGen . newIORef

instance RandomGen g => StatefulRandomGen g (AtomicGen g) IO where
  withRandomState f (AtomicGen ref) = atomicModifyIORef' ref (swap . f)
