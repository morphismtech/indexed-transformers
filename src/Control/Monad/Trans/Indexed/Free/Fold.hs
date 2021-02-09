{-# LANGUAGE
    FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free.Fold
  ( FreeIx (..)
  ) where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free
import Data.Silo
import Data.Silo.Functor

newtype FreeIx g i j m x = FreeIx
  {runFreeIx :: forall t. (IndexedMonadTrans t, Monad m)
    => (forall i j x. g i j x -> t i j m x) -> t i j m x}
instance (Silo g, Monad m) => Functor (FreeIx g i j m) where
  fmap f (FreeIx k) = FreeIx $ \step -> fmap f (k step)
instance (Silo f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure x = FreeIx $ const $ pure x
    (<*>) = ixAp
instance (Silo f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return x = FreeIx $ const $ return x
    (>>=) = flip ixBind
instance (Silo f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift m = FreeIx $ const $ lift m
instance Silo f
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx g) = FreeIx $ \k -> ixBind (\(FreeIx f) -> f k) (g k)
instance SFunctor FreeIx where
  smap f (FreeIx k) = FreeIx $ \g -> k (g . f)
instance SFoldable FreeIx where
  sfoldMap f (FreeIx k) = k f
instance SPointed FreeIx where
  slift m = FreeIx $ \k -> k m
instance SMonad FreeIx
instance
  ( Silo f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = join . slift
instance IxFree FreeIx
