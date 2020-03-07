{-# LANGUAGE
    DeriveFunctor
  , GADTs
  , PolyKinds
#-}

module Control.Monad.Trans.Indexed.Ix
  ( Ix (..)
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Indexed

newtype Ix i j m x = Ix {runIx :: m x}
  deriving Functor

instance IndexedMonadTrans Ix where
  ixJoin (Ix m) = Ix $ do
    Ix n <- m
    n
instance (i ~ j, Applicative m) => Applicative (Ix i j m) where
  pure = Ix . pure
  Ix mf <*> Ix mx = Ix $ mf <*> mx
instance (i ~ j, Monad m) => Monad (Ix i j m) where
  return = Ix . return
  (>>=) = flip ixBind
instance i ~ j => MonadTrans (Ix i j) where
  lift = Ix
