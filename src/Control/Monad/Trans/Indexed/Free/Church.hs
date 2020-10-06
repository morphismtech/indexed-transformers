{-# LANGUAGE
    GADTs
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free.Church
  ( FreeIx (..)
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free

newtype FreeIx g i j m x = FreeIx
  {getFreeIx :: forall t. (IndexedMonadTrans t, Monad m)
    => (forall i j x. g i j x -> t i j m x) -> t i j m x}
instance (Silo g, Monad m) => Functor (FreeIx g i j m) where
  fmap f (FreeIx k) = FreeIx $ \step -> fmap f (k step)
instance (Silo f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure x = FreeIx $ const $ pure x
    (<*>) = undefined
instance (Silo f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return x = FreeIx $ const $ return x
    (>>=) = undefined
instance (Silo f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift m = FreeIx $ const $ lift m
-- instance Silo f
--   => IndexedMonadTrans (FreeIx f) where
--     ixJoin (FreeIx k) = FreeIx $ \j -> j _
