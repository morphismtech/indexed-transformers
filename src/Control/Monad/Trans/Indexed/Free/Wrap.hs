{-# LANGUAGE
    ConstraintKinds
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , QuantifiedConstraints
  , MultiParamTypeClasses
  , PolyKinds
#-}

module Control.Monad.Trans.Indexed.Free.Wrap
  ( FreeIx (..)
  , WrapIx (..)
  ) where

import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free

data WrapIx f i j m x where
  Unwrap :: x -> WrapIx f i i m x
  Wrap :: f i j (FreeIx f j k m x) -> WrapIx f i k m x
instance (IxFunctor f, Monad m)
  => Functor (WrapIx f i j m) where
    fmap f = \case
      Unwrap x -> Unwrap $ f x
      Wrap fm -> Wrap $ fmap (fmap f) fm

newtype FreeIx f i j m x = FreeIx {runFreeIx :: m (WrapIx f i j m x)}
instance (IxFunctor f, Monad m)
  => Functor (FreeIx f i j m) where
    fmap f (FreeIx m) = FreeIx $ fmap (fmap f) m
instance (IxFunctor f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure = FreeIx . pure . Unwrap
    (<*>) = ixAp
instance (IxFunctor f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = pure
    (>>=) = flip ixBind
instance (IxFunctor f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift = FreeIx . fmap Unwrap
instance IxFunctor f
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx mm) = FreeIx $ mm >>= \case
      Unwrap (FreeIx m) -> m
      Wrap fm -> return $ Wrap $ fmap ixJoin fm
instance
  ( IxFunctor f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = FreeIx . return . Wrap
instance IxFree FreeIx where
  ixlift = FreeIx . return . Wrap . fmap return
  ixhoist f (FreeIx m) = FreeIx (fmap hoist_f m)
    where
      hoist_f = \case
        Unwrap x -> Unwrap x
        Wrap y -> Wrap (f (fmap (ixhoist f) y))
  ixfoldMap f (FreeIx m) = ixBind foldMap_f (lift m)
    where
      foldMap_f = \case
        Unwrap x -> return x
        Wrap y -> ixBind (ixfoldMap f) (f y)
