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

module Control.Monad.Trans.Indexed.Free.Lance
  ( FreeIx (..)
  , Lance (..)
  ) where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free
import qualified Control.Monad.Trans.Indexed.Free.Wrap as Wrap

data Lance f i j x where
  Lance :: (x -> y) -> f i j x -> Lance f i j y
instance Functor (Lance f i j) where
  fmap g (Lance f x) = Lance (g . f) x
mapLance :: (forall x. f i j x -> g i j x) -> Lance f i j x -> Lance g i j x
mapLance g (Lance f x) = Lance f (g x)
foldLance
  :: (IndexedMonadTrans t, Monad m, Silo g)
  => (forall x. g i j x -> t i j m x)
  -> Lance g i j x -> t i j m x
foldLance g (Lance f x) = g (fmap f x)
liftLance :: g i j x -> Lance g i j x
liftLance x = Lance id x

newtype FreeIx f i j m x = FreeIx {runFreeIx :: Wrap.FreeIx (Lance f) i j m x}
deriving newtype instance Monad m => Functor (FreeIx f i j m)
instance SFunctor FreeIx where
  smap f (FreeIx wrapped) = FreeIx (smap (mapLance f) wrapped)
instance SFoldable FreeIx where
  sfoldMap f (FreeIx wrapped) = sfoldMap (foldLance f) wrapped
instance SPointed FreeIx where
  slift = FreeIx . slift . liftLance
instance SMonad FreeIx where
  sbind = sfoldMap
instance (i ~ j, Monad m) => Applicative (FreeIx f i j m) where
  pure = FreeIx . Wrap.FreeIx . return . Wrap.Unwrap
  (<*>) = ixAp
instance (i ~ j, Monad m) => Monad (FreeIx f i j m) where
  return = FreeIx . Wrap.FreeIx . return . Wrap.Unwrap
  (>>=) = flip ixBind
instance i ~ j => MonadTrans (FreeIx f i j) where
  lift = FreeIx . Wrap.FreeIx . fmap Wrap.Unwrap
instance IndexedMonadTrans (FreeIx f) where
  ixBind g (FreeIx (Wrap.FreeIx m)) = do
    FreeIx . Wrap.FreeIx $ m >>= Wrap.runFreeIx . \case
      Wrap.Unwrap x -> runFreeIx (g x)
      Wrap.Wrap (Lance f y) ->
        ixBind (ixAndThen (runFreeIx . g) f) (slift (liftLance y))
instance
  ( Silo f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = join . slift
instance IxFree FreeIx
