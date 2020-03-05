{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , TupleSections
#-}

module Control.Monad.Trans.Indexed.State
  ( StateIx (..)
  , toStateT
  , fromStateT
  ) where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Indexed

newtype StateIx i j m x = StateIx { runStateIx :: i -> m (x, j)}
  deriving Functor
instance IndexedMonadTrans StateIx where
  ixJoin (StateIx f) = StateIx $ \i -> do
    (StateIx g, j) <- f i
    g j
instance (i ~ j, Monad m) => Applicative (StateIx i j m) where
  pure x = StateIx $ \i -> pure (x, i)
  (<*>) = ixAp
instance (i ~ j, Monad m) => Monad (StateIx i j m) where
  return x = StateIx $ \i -> return (x, i)
  (>>=) = flip ixBind
instance i ~ j => MonadTrans (StateIx i j) where
  lift m = StateIx $ \i -> (, i) <$> m
instance (i ~ j, Monad m) => MonadState i (StateIx i j m) where
  state f = StateIx (return . f)

toStateT :: StateIx i i m x -> StateT i m x
toStateT (StateIx f) = StateT f

fromStateT :: StateT i m x -> StateIx i i m x
fromStateT (StateT f) = StateIx f
