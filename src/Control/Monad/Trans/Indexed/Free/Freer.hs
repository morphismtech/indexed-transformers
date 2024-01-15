{-# LANGUAGE
    DerivingStrategies
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , QuantifiedConstraints
  , RankNTypes
  , StandaloneDeriving
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free.Freer
  ( FreeIx (..)
  ) where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free
import Data.Silo
import Data.Silo.Functor

import qualified Control.Monad.Trans.Indexed.Free.Wrap as Wrap

newtype FreeIx f i j m x = FreeIx {runFreeIx :: Wrap.FreeIx (Lance f) i j m x}
deriving newtype instance Monad m => Functor (FreeIx f i j m)
instance SFunctor FreeIx where
  smap f (FreeIx wrapped) = FreeIx (smap (mapLance f) wrapped)
instance SFoldable FreeIx where
  sfoldMap f (FreeIx wrapped) = sfoldMap (f . lowerLance) wrapped
instance SPointed FreeIx where
  slift = FreeIx . slift . liftLance
instance SMonad FreeIx
instance (i ~ j, Monad m) => Applicative (FreeIx f i j m) where
  pure = FreeIx . Wrap.FreeIx . return . Wrap.Unwrap
  (<*>) = ixAp
instance (i ~ j, Monad m) => Monad (FreeIx f i j m) where
  return = pure
  (>>=) = flip ixBind
instance i ~ j => MonadTrans (FreeIx f i j) where
  lift = FreeIx . Wrap.FreeIx . fmap Wrap.Unwrap
instance IndexedMonadTrans (FreeIx f) where
  ixJoin (FreeIx (Wrap.FreeIx m)) =
    FreeIx . Wrap.FreeIx $ m >>= \case
      Wrap.Unwrap y -> Wrap.runFreeIx (runFreeIx y)
      Wrap.Wrap (Lance f x) -> return $
        Wrap.Wrap (Lance (ixAndThen runFreeIx f) x)
instance
  ( Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = join . FreeIx . slift . liftLance
instance IxFree FreeIx
