{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , TupleSections
#-}

module Control.Monad.Trans.Indexed.State
  ( StateIx (..)
  , evalStateIx
  , execStateIx
  , modifyIx
  , putIx
  , toStateT
  , fromStateT
  ) where

import Control.Monad.State
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
  return = pure
  (>>=) = flip ixBind
instance i ~ j => MonadTrans (StateIx i j) where
  lift m = StateIx $ \i -> (, i) <$> m
instance (i ~ j, Monad m) => MonadState i (StateIx i j m) where
  state f = StateIx (return . f)

evalStateIx :: Monad m => StateIx i j m x -> i -> m x
evalStateIx m i = fst <$> runStateIx m i

execStateIx :: Monad m => StateIx i j m x -> i -> m j
execStateIx m i = snd <$> runStateIx m i

modifyIx :: Applicative m => (i -> j) -> StateIx i j m ()
modifyIx f = StateIx $ \i -> pure ((), f i)

putIx :: Applicative m => j -> StateIx i j m ()
putIx j = modifyIx (\ _ -> j)

toStateT :: StateIx i i m x -> StateT i m x
toStateT (StateIx f) = StateT f

fromStateT :: StateT i m x -> StateIx i i m x
fromStateT (StateT f) = StateIx f
