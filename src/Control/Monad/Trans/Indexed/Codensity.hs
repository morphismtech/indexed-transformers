{-# LANGUAGE
DerivingStrategies
, GeneralizedNewtypeDeriving
, StandaloneDeriving
, TypeApplications
#-}

module Control.Monad.Trans.Indexed.Codensity
  ( CodensityIx (..)
  , lowerCodensityIx
  , liftCodensityIx
  , ReaderIx
  , KanStateIx
  , ReadStx (..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free
import Control.Monad.Trans.Indexed.Free.Fold
import Control.Monad.Trans.Indexed.State hiding (StateIx (..))

newtype CodensityIx t i j m a = CodensityIx
  { runCodensityIx :: forall b k. (a -> t j k m b) -> t i k m b }
  deriving Functor

lowerCodensityIx
  :: (IxMonadTrans t, Monad m)
  => CodensityIx t i j m a -> t i j m a
lowerCodensityIx (CodensityIx f) = f return

liftCodensityIx
  :: (IxMonadTrans t, Monad m)
  => t i j m a -> CodensityIx t i j m a
liftCodensityIx m = CodensityIx $ \h -> bindIx h m

instance IxMonadTrans t => IxMonadTrans (CodensityIx t) where
  joinIx (CodensityIx k) =
    CodensityIx $ \f -> k $ \(CodensityIx g) -> g f
instance i ~ j => Applicative (CodensityIx t i j m) where
  pure x = CodensityIx $ \k -> k x
  CodensityIx cf <*> CodensityIx cx =
    CodensityIx $ \ k -> cf $ \ f -> cx (k . f)
instance i ~ j => Monad (CodensityIx t i j m) where
  return = pure
  CodensityIx cx >>= k =
    CodensityIx $ \ c -> cx (\ x -> runCodensityIx (k x) c)
instance (IxMonadTrans t, i ~ j) => MonadTrans (CodensityIx t i j) where
  lift m = CodensityIx (\k -> bindIx k (lift m))
instance (i ~ j, Alternative (t i j m), IxMonadTrans t, Monad m)
  => Alternative (CodensityIx t i j m) where
    empty = liftCodensityIx empty
    x <|> y = liftCodensityIx (lowerCodensityIx x <|> lowerCodensityIx y)
instance (i ~ j, Alternative (t i j m), IxMonadTrans t, Monad m)
  => MonadPlus (CodensityIx t i j m)
instance (i ~ j, IxMonadTransReader t, Monad m)
  => MonadState i (CodensityIx t i j m) where
  get = liftCodensityIx ask
  put = putIx
instance IxMonadTransReader t => IxMonadTransState (CodensityIx t) where
  putIx s = CodensityIx (localIx (const s) . ($ ()))

class
  ( IxMonadTrans t
  , forall m i j r. (Monad m, i ~ j, j ~ r) => MonadReader r (t i j m)
  ) => IxMonadTransReader t where
  localIx :: Monad m => (i -> h) -> t h j m a -> t i j m a

type ReaderIx = FreeIx (Ixer ReadStx)

type KanStateIx = CodensityIx ReaderIx

data ReadStx s t x where AskStx :: ReadStx s s s

instance (s ~ t, Monad m, IxMonadTransFree freeIx)
  => MonadReader s (freeIx (Ixer ReadStx) s t m) where
    ask = liftFreerIx AskStx
    local = localIx
instance IxMonadTransFree freeIx
  => IxMonadTransReader (freeIx (Ixer ReadStx)) where
    localIx f = lowerCodensityIx . (\m -> bindIx (thenIx m . putIx . f) get) . liftCodensityIx
