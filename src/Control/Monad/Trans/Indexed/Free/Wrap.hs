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
import Data.Silo
import Data.Silo.Functor

data WrapIx f i j m x where
  Unwrap :: x -> WrapIx f i i m x
  Wrap :: f i j (FreeIx f j k m x) -> WrapIx f i k m x
instance (Silo f, Monad m)
  => Functor (WrapIx f i j m) where
    fmap f = \case
      Unwrap x -> Unwrap $ f x
      Wrap fm -> Wrap $ fmap (fmap f) fm
instance SFunctor WrapIx where
  smap f = \case
    Unwrap x -> Unwrap x
    Wrap y -> Wrap (f (fmap (smap f) y))
instance SFoldable WrapIx where
  sfoldMap f = \case
    Unwrap x -> return x
    Wrap y -> ixBind (sfoldMap f) (f y)
instance SPointed WrapIx where
  slift = Wrap . fmap return

newtype FreeIx f i j m x = FreeIx {runFreeIx :: m (WrapIx f i j m x)}
instance SFunctor FreeIx where
  smap f (FreeIx m) = FreeIx (fmap (smap f) m)
instance SFoldable FreeIx where
  sfoldMap f (FreeIx m) = ixBind (sfoldMap f) (lift m)
instance SPointed FreeIx where
  slift = FreeIx . return . slift
instance SMonad FreeIx
instance (Silo f, Monad m)
  => Functor (FreeIx f i j m) where
    fmap f (FreeIx m) = FreeIx $ fmap (fmap f) m
instance (Silo f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure = FreeIx . pure . Unwrap
    (<*>) = ixAp
instance (Silo f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = FreeIx . return . Unwrap
    (>>=) = flip ixBind
instance (Silo f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift = FreeIx . fmap Unwrap
instance Silo f
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx mm) = FreeIx $ mm >>= \case
      Unwrap (FreeIx m) -> m
      Wrap fm -> return $ Wrap $ fmap ixJoin fm
instance
  ( Silo f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = FreeIx . return . Wrap
instance IxFree FreeIx
