{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppName.Gateways.CryptoRandomGen
  ( Ref,
    newRef,
    withRef,
  )
where

import qualified Basement.Block as Block
import qualified Basement.PrimType as Prim
import Basement.Types.OffsetSize (CountOf (..), Offset (..))
import qualified Crypto.Random
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import System.Random (RandomGen (..))

-- | A thread-safe, atomic reference to keep the state of a
-- cryptographic random number generator. Use it when it is crucial to
-- make it practically impossible to predict random numbers, having
-- got some of them, e.g. to generate one-time passwords, tokens, etc.
newtype Ref = Ref (IORef DRGRandomGen)

-- | Creates a new random state reference, initialized with a random seed.
newRef :: IO Ref
newRef = do
  chaChaDRG <- Crypto.Random.drgNew
  Ref <$> newIORef (DRGRandomGen chaChaDRG)

-- | Performs a pure computation over the random number generator,
-- managing its state within the IO monad. Example:
--
-- @
--     randomDigit <- withRef randomRef $ randomR (0, 9)
-- @
withRef :: Ref -> (forall gen. RandomGen gen => gen -> (a, gen)) -> IO a
withRef (Ref ref) f = atomicModifyIORef' ref (swap . f)

-- | This adapts whatever instance of 'DRG' class to 'RandomGen'
-- class.
data DRGRandomGen = forall gen. Crypto.Random.DRG gen => DRGRandomGen gen

instance RandomGen DRGRandomGen where
  next = generateInt
  split = error "DRGRandomGen: cannot split, the generator is not splittable"

generateInt :: DRGRandomGen -> (Int, DRGRandomGen)
generateInt (DRGRandomGen drg) = (newInt, DRGRandomGen drg')
  where
    (blockOfInt, drg') = generateRandomBlock 1 drg
    newInt = blockOfInt `Block.index` Offset 0

generateRandomBlock :: forall gen ty. (Crypto.Random.DRG gen, Prim.PrimType ty, Ord ty) => Int -> gen -> (Block.Block ty, gen)
generateRandomBlock numElements = Crypto.Random.randomBytesGenerate (elementSize * numElements)
  where
    (CountOf elementSize) = Prim.primSizeInBytes (Proxy :: Proxy ty)
