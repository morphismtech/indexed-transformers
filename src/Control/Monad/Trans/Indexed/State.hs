{- |
Module      :  Control.Monad.Trans.Indexed.State
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The state indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.State
  ( StateIx (..)
  , evalStateIx
  , execStateIx
  , toStateT
  , fromStateT
  , IxMonadTransReader (..)
  , IxMonadTransState (..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed

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
instance (i ~ j, Monad m) => MonadReader i (StateIx i j m) where
  ask = get
  local = undefined

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
