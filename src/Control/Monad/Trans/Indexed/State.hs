{- |
Module      :  Control.Monad.Trans.Indexed.State
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The state indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.State
  ( -- * State
    IxMonadTransState (..)
  , StateIx (..)
  , evalStateIx
  , execStateIx
  , toStateT
  , fromStateT
    -- * Reader
  , IxMonadTransReader (..)
  , ReaderIx
  , ReadStx (..)
    -- * Codensity
  , CodensityIx (..)
  , liftCodensityIx
  , lowerCodensityIx
  , fromStateIx
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free
import Control.Monad.Trans.Indexed.Free.Wrap

newtype StateIx i j m x = StateIx {runStateIx :: i -> m (x, j)}
  deriving Functor
instance IxMonadTrans StateIx where
  joinIx (StateIx f) = StateIx $ \i -> do
    (StateIx g, j) <- f i
    g j
instance (i ~ j, Monad m) => Applicative (StateIx i j m) where
  pure x = StateIx $ \i -> pure (x, i)
  (<*>) = apIx
instance (i ~ j, Monad m) => Monad (StateIx i j m) where
  return = pure
  (>>=) = flip bindIx
instance i ~ j => MonadTrans (StateIx i j) where
  lift m = StateIx $ \i -> (, i) <$> m
instance (i ~ j, Monad m) => MonadState i (StateIx i j m) where
  state f = StateIx (return . f)

evalStateIx :: Monad m => StateIx i j m x -> i -> m x
evalStateIx m i = fst <$> runStateIx m i

execStateIx :: Monad m => StateIx i j m x -> i -> m j
execStateIx m i = snd <$> runStateIx m i

toStateT :: StateIx i i m x -> StateT i m x
toStateT (StateIx f) = StateT f

fromStateT :: StateT i m x -> StateIx i i m x
fromStateT (StateT f) = StateIx f

class
  ( IxMonadTrans t
  , forall m i j s. (Monad m, i ~ j, j ~ s) => MonadState s (t i j m)
  ) => IxMonadTransState t where
  putIx :: Monad m => j -> t i j m ()
  putIx s = stateIx (\_ -> ((), s))
  modifyIx :: Monad m => (i -> j) -> t i j m ()
  modifyIx f = stateIx (\i -> ((), f i))
  stateIx :: Monad m => (i -> (a,j)) -> t i j m a
  stateIx f =
    bindIx (\s -> let ~(a, s') = f s in thenIx (return a) (putIx s')) get
instance IxMonadTransState StateIx where
  stateIx f = StateIx (return . f)

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

fromStateIx :: Monad m => StateIx i j m x -> CodensityIx ReaderIx i j m x
fromStateIx (StateIx f) = get & bindIx
  (bindIx (\(x,j) -> putIx j & thenIx (return x)) . lift . f)

class
  ( IxMonadTrans t
  , forall m i j r. (Monad m, i ~ j, j ~ r) => MonadReader r (t i j m)
  ) => IxMonadTransReader t where
  localIx :: Monad m => (i -> h) -> t h j m a -> t i j m a

type ReaderIx = FreeIx (Ixer ReadStx)

data ReadStx s t x where AskStx :: ReadStx s s s

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

instance (s ~ t, Monad m, IxMonadTransFree freeIx)
  => MonadReader s (freeIx (Ixer ReadStx) s t m) where
    ask = liftFreerIx AskStx
    local = localIx
instance IxMonadTransFree freeIx
  => IxMonadTransReader (freeIx (Ixer ReadStx)) where
    localIx f
      = lowerCodensityIx
      . (\m -> bindIx (thenIx m . putIx . f) get)
      . liftCodensityIx
