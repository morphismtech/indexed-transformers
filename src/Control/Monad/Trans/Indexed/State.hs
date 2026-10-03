{- |
Module      :  Control.Monad.Trans.Indexed.State
Copyright   :  (C) 2026 Eitan Chatav
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
  , IxMonadTransState (..), modifyIx
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Do qualified as Indexed

newtype StateIx i j m x = StateIx { runStateIx :: i -> m (x, j)}
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
  , forall i m. Monad m => MonadState i (t i i m)
  ) => IxMonadTransState t where
  {-# MINIMAL stateIx | getIx, putIx #-}
  getIx :: Monad m => t i i m i
  getIx = stateIx (\i -> return (i,i))
  putIx :: Monad m => j -> t i j m ()
  putIx i = stateIx (\_ -> return ((),i))
  stateIx :: Monad m => (i -> m (x,j)) -> t i j m x
  stateIx f = Indexed.do
    i <- getIx
    ~(x, j) <- lift (f i)
    putIx j
    return x
instance IxMonadTransState StateIx where
  stateIx = StateIx
modifyIx :: (IxMonadTransState t, Monad m) => (i -> j) -> t i j m ()
modifyIx f = stateIx (\i -> return ((), f i))
