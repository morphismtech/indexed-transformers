{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , QuantifiedConstraints
  , RankNTypes
#-}

module Control.Monad.Trans.Indexed.Free
  ( FreeIx (..)
  , FreeIxF (..)
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Indexed

data FreeIxF f i j m x where
  PureIx :: x -> FreeIxF f i i m x
  JoinIx :: f i j (FreeIx f j k m x) -> FreeIxF f i k m x

newtype FreeIx f i j m x = FreeIx {runFreeIx :: m (FreeIxF f i j m x)}
instance (forall i j. Functor (f i j), Monad m)
  => Functor (FreeIx f i j m) where
    fmap f (FreeIx m) = FreeIx $ m >>= \case
      PureIx x -> return $ PureIx (f x)
      JoinIx fm -> return $ JoinIx $ fmap (fmap f) fm
instance (forall i j. Functor (f i j), i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure = FreeIx . pure . PureIx
    (<*>) = ixAp
instance (forall i j. Functor (f i j), i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = FreeIx . pure . PureIx
    (>>=) = flip ixBind
instance (forall i j. Functor (f i j), i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift = FreeIx . fmap PureIx
instance (forall i j. Functor (f i j))
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx mm) = FreeIx $ mm >>= \case
      PureIx (FreeIx m) -> m
      JoinIx fm -> return $ JoinIx $ fmap ixJoin fm
