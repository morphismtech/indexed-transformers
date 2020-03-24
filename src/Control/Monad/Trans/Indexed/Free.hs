{-# LANGUAGE
    DataKinds
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

module Control.Monad.Trans.Indexed.Free
  ( IndexedMonadTransFree (..)
  , liftIxF
  , FreeIx (..)
  , FreeIxF (..)
  ) where

import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Data.Function ((&))

class IndexedMonadTrans t => IndexedMonadTransFree f t | t -> f where
  wrapIx :: Monad m => f i j (t j k m x) -> t i k m x

liftIxF
  :: ( forall i j. Functor (f i j)
     , IndexedMonadTransFree f t
     , Monad m
     ) => f i j x -> t i j m x
liftIxF = wrapIx . fmap return

data FreeIxF f i j m x where
  PureIx :: x -> FreeIxF f i i m x
  WrapIx :: f i j (FreeIx f j k m x) -> FreeIxF f i k m x
instance (forall i j. Functor (f i j), Monad m)
  => Functor (FreeIxF f i j m) where
    fmap f = \case
      PureIx x -> PureIx $ f x
      WrapIx fm -> WrapIx $ fmap (fmap f) fm

newtype FreeIx f i j m x = FreeIx {runFreeIx :: m (FreeIxF f i j m x)}
instance (forall i j. Functor (f i j), Monad m)
  => Functor (FreeIx f i j m) where
    fmap f (FreeIx m) = FreeIx $ fmap (fmap f) m
instance (forall i j. Functor (f i j), i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure = FreeIx . pure . PureIx
    (<*>) = ixAp
instance (forall i j. Functor (f i j), i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = FreeIx . return . PureIx
    (>>=) = flip ixBind
instance (forall i j. Functor (f i j), i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift = FreeIx . fmap PureIx
instance (forall i j. Functor (f i j))
  => IndexedMonadTrans (FreeIx f) where
    ixJoin (FreeIx mm) = FreeIx $ mm >>= \case
      PureIx (FreeIx m) -> m
      WrapIx fm -> return $ WrapIx $ fmap ixJoin fm
instance
  ( forall i j. Functor (f i j)
  ) => IndexedMonadTransFree f (FreeIx f) where
  wrapIx = FreeIx . return . WrapIx
instance
  ( forall i j. Functor (f i j)
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = wrapIx
