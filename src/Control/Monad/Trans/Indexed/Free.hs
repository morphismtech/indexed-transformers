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

module Control.Monad.Trans.Indexed.Free
  ( IxFree
  , Silo
  , SFunctor (..)
  , SPointed (..)
  , SFoldable (..)
  , SMonad (..)
  ) where

import Control.Monad.Trans.Indexed

class
  ( SFoldable f
  , SMonad f
  , forall g. Silo g => IndexedMonadTrans (f g)
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
