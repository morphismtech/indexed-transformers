{-# LANGUAGE
    ConstraintKinds
  , GADTs
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , UndecidableInstances
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free
  ( IxFree
  , Silo
  , SFunctor (..)
  , SPointed (..)
  , SFoldable (..)
  , SMonad (..)
  , toIxFree
  ) where

import Control.Monad.Free
import Control.Monad.Trans.Indexed

class
  ( SFoldable f
  , SMonad f
  , forall g. Silo g => IndexedMonadTrans (f g)
  , forall g m i j. (Silo g, Monad m, i ~ j) => MonadFree (g i j) (f g i j m)
  ) => IxFree f where

type Silo g = forall i j. Functor (g i j)

class SFunctor f where
  smap
    :: (Silo g, Silo h, Monad m)
    => (forall i j x. g i j x -> h i j x)
    -> f g i j m x -> f h i j m x

class SFunctor f => SPointed f where
  slift :: (Silo g, Monad m) => g i j x -> f g i j m x

class SFunctor f => SFoldable f where
  sfoldMap
    :: (Silo g, IndexedMonadTrans t, Monad m)
    => (forall i j x. g i j x -> t i j m x)
    -> f g i j m x -> t i j m x

class SPointed f => SMonad f where
  sbind
    :: (Silo g, Silo h, Monad m)
    => (forall i j x. g i j x -> f h i j m x)
    -> f g i j m x -> f h i j m x

toIxFree
  :: (Silo g, Monad m, SFoldable f0, IxFree f1)
  => f0 g i j m x -> f1 g i j m x 
toIxFree = sfoldMap slift
