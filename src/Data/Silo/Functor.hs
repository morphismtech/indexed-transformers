{-# LANGUAGE
    DefaultSignatures
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Silo.Functor
  ( SFunctor (..)
  , SPointed (..)
  , SFoldable (..)
  , SMonad (..)
  ) where

import Control.Monad.Trans.Indexed
import Data.Silo

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
  default sbind
    :: (Silo g, Silo h, IndexedMonadTrans (f h), Monad m, SFoldable f)
    => (forall i j x. g i j x -> f h i j m x)
    -> f g i j m x -> f h i j m x
  sbind = sfoldMap
