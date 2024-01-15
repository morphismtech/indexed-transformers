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

newtype FreeIx g i j m x = FreeIx
  {runFreeIx :: forall t. (IndexedMonadTrans t, Monad m)
    => (forall i j x. g i j x -> t i j m x) -> t i j m x}
instance (IxFunctor f, Monad m) => Functor (FreeIx f i j m) where
  fmap f (FreeIx k) = FreeIx $ \step -> fmap f (k step)
instance (IxFunctor f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure x = FreeIx $ const $ pure x
    (<*>) = ixAp
instance (IxFunctor f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = pure
    (>>=) = flip ixBind
instance (IxFunctor f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift m = FreeIx $ const $ lift m
instance IxFunctor f
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx g) = FreeIx $ \k -> ixBind (\(FreeIx f) -> f k) (g k)
instance
  ( IxFunctor f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = join . ixlift
instance IxFree FreeIx where
  ixlift m = FreeIx $ \k -> k m
  ixhoist f (FreeIx k) = FreeIx $ \g -> k (g . f)
  ixfoldMap f (FreeIx k) = k f
