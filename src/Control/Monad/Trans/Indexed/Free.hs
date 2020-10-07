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
  , coerceIxFree
  ) where

import Control.Monad.Free
import Control.Monad.Trans.Indexed

class
  ( SFoldable f
  , SMonad f
  , forall g. Silo g => IndexedMonadTrans (f g)
  , forall g m i j. (Silo g, Monad m, i ~ j) => MonadFree (g i j) (f g i j m)
  ) => IxFree f where

{- |
A `Silo` can be a DSL describing primitive commands like
[this Conor McBride example]
(https://stackoverflow.com/questions/28690448/what-is-indexed-monad).

>>> type DVD = String
>>> :{
data DVDCommand :: Bool -> Bool -> * -> * where -- Bool is "drive full?"
  Insert :: x -> DVD -> DVDCommand False True x
  Eject :: (DVD -> x) -> DVDCommand True False x
:}
>>> deriving instance Functor (DVDCommand i j)

`DVDCommand` is a `Silo` which can be lifted to an `IxFree`.

>>> :{
let
  insert :: (IxFree free, Monad m) => DVD -> free DVDCommand False True m ()
  insert dvd = slift (Insert () dvd)
  eject :: (IxFree free, Monad m) => free DVDCommand True False m DVD
  eject = slift (Eject id)
  discSwap :: (IxFree free, Monad m) => DVD -> free DVDCommand True True m DVD
  discSwap dvd = eject & ixBind (\dvd' -> insert dvd & ixThen (return dvd'))
:}
-}
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

coerceIxFree
  :: (IxFree f0, IxFree f1, Silo g, Monad m)
  => f0 g i j m x -> f1 g i j m x 
coerceIxFree = sfoldMap slift
