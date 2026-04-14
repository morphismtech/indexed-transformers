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
  , IxMonadTransState (..)
  , IxMonadTransReader (..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed

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
  state = stateIx
instance (i ~ j, Monad m) => MonadReader i (StateIx i j m) where
  reader f = StateIx (\i -> return (f i,i))
  local = localIx
instance IxMonadTransReader StateIx where
  localIx hi (StateIx ij) = StateIx $ \h -> ij (hi h)
instance IxMonadTransState StateIx where
  stateIx f = StateIx (return . f)

evalStateIx :: Monad m => StateIx i j m x -> i -> m x
evalStateIx m i = fst <$> runStateIx m i

execStateIx :: Monad m => StateIx i j m x -> i -> m j
execStateIx m i = snd <$> runStateIx m i

toStateT :: StateIx i i m x -> StateT i m x
toStateT (StateIx f) = StateT f

fromStateT :: StateT i m x -> StateIx i i m x
fromStateT (StateT f) = StateIx f

class
  ( forall r i j m. (r ~ i, i ~ j, Monad m) => MonadReader r (t i j m)
  , IxMonadTrans t
  ) => IxMonadTransReader t where
    localIx :: Monad m => (h -> i) -> t i j m x -> t h j m x

class
  ( forall s i j m. (s ~ i, i ~ j, Monad m) => MonadState s (t i j m)
  , IxMonadTransReader t
  ) => IxMonadTransState t where
    putIx :: Monad m => j -> t i j m ()
    putIx j = modifyIx $ \_ -> j
    modifyIx :: Monad m => (i -> j) -> t i j m ()
    modifyIx f = stateIx $ \i -> ((), f i)
    stateIx :: Monad m => (i -> (x,j)) -> t i j m x
    stateIx f = fmap f get & bindIx (\(x,j) -> putIx j & thenIx (return x))
    withStateIx :: Monad m => (j -> k) -> t i j m x -> t i k m x
    withStateIx f m = m & bindIx (\x -> modifyIx f & thenIx (return x))
