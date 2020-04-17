{-# LANGUAGE
    PolyKinds
  , QuantifiedConstraints
  , RankNTypes
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free.Church
  ( FreeIx (..)
  ) where

import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free

newtype FreeIx g i j m x = FreeIx
  {getFreeIx :: forall t m. (IndexedMonadTrans t, Monad m)
    => (forall i j x. g i j x -> t i j m x) -> t i j m x}
instance (Silo g, Monad m) => Functor (FreeIx g i j m) where
  fmap f (FreeIx k) = FreeIx $ \step -> fmap f (k step)
