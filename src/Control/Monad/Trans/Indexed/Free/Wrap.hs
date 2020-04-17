{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , DeriveFunctor
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , StandaloneDeriving
  , UndecidableInstances
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free.Wrap
  ( FreeIx (..)
  , WrapIx (..)
  ) where

import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free

data WrapIx f i j m x where
  Unwrapped :: x -> WrapIx f i i m x
  Wrapped :: f i j (FreeIx f j k m x) -> WrapIx f i k m x
instance (forall i j. Functor (f i j), Monad m)
  => Functor (WrapIx f i j m) where
    fmap f = \case
      Unwrapped x -> Unwrapped $ f x
      Wrapped fm -> Wrapped $ fmap (fmap f) fm
instance SFunctor WrapIx where
  smap f = \case
    Unwrapped x -> Unwrapped x
    Wrapped y -> Wrapped (f (fmap (smap f) y))
instance SFoldable WrapIx where
  sfoldMap f = \case
    Unwrapped x -> return x
    Wrapped y -> ixBind (sfoldMap f) (f y)
instance SPointed WrapIx where
  slift = Wrapped . fmap return

newtype FreeIx f i j m x = FreeIx {runFreeIx :: m (WrapIx f i j m x)}
instance SFunctor FreeIx where
  smap f (FreeIx m) = FreeIx (fmap (smap f) m)
instance SFoldable FreeIx where
  sfoldMap f (FreeIx m) = ixBind (sfoldMap f) (lift m)
instance SPointed FreeIx where
  slift = FreeIx . return . slift
instance (forall i j. Functor (f i j), Monad m)
  => Functor (FreeIx f i j m) where
    fmap f (FreeIx m) = FreeIx $ fmap (fmap f) m
instance (forall i j. Functor (f i j), i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure = FreeIx . pure . Unwrapped
    (<*>) = ixAp
instance (forall i j. Functor (f i j), i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = FreeIx . return . Unwrapped
    (>>=) = flip ixBind
instance (forall i j. Functor (f i j), i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift = FreeIx . fmap Unwrapped
instance (forall i j. Functor (f i j))
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx mm) = FreeIx $ mm >>= \case
      Unwrapped (FreeIx m) -> m
      Wrapped fm -> return $ Wrapped $ fmap ixJoin fm
instance
  ( forall i j. Functor (f i j)
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = FreeIx . return . Wrapped
