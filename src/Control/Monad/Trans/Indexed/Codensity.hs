{- |
Module      :  Control.Monad.Trans.Indexed.Codensity
Copyright   :  (C) 2026 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The indexed codensity monad transformer.
-}

module Control.Monad.Trans.Indexed.Codensity
  ( CodensityIx (..)
  , lowerCodensityIx
  , liftCodensityIx
  , toCodensity
  , wrapCodensityIx
  , resetIx
  , shiftIx
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Monad.Trans.Indexed
import Data.Kind

newtype CodensityIx t i j m a = CodensityIx
  { runCodensityIx :: forall b k. (a -> t j k m b) -> t i k m b }
  deriving Functor

lowerCodensityIx
  :: (IxMonadTrans t, Monad m)
  => CodensityIx t i j m a -> t i j m a
lowerCodensityIx (CodensityIx f) = f return

liftCodensityIx
  :: (IxMonadTrans t, Monad m)
  => t i j m a -> CodensityIx t i j m a
liftCodensityIx m = CodensityIx $ \h -> bindIx h m

toCodensity :: CodensityIx t i i m a -> Codensity (t i i m) a
toCodensity (CodensityIx f) = Codensity f

wrapCodensityIx :: (forall a k. t j k (m :: Type -> Type) a -> t i k m a) -> CodensityIx t i j m ()
wrapCodensityIx f = CodensityIx (\k -> f (k ()))

resetIx :: (IxMonadTrans t, Monad m) => CodensityIx t i j m a -> CodensityIx t i j m a
resetIx = liftCodensityIx . lowerCodensityIx

shiftIx
  :: (IxMonadTrans t, Monad m)
  => (forall b k. (a -> t i k m b) -> CodensityIx t i k m b)
  -> CodensityIx t i i m a
shiftIx f = CodensityIx $ lowerCodensityIx . f

instance IxMonadTrans t => IxMonadTrans (CodensityIx t) where
  joinIx (CodensityIx k) =
    CodensityIx $ \f -> k $ \(CodensityIx g) -> g f
instance i ~ j => Applicative (CodensityIx t i j m) where
  pure x = CodensityIx $ \k -> k x
  CodensityIx cf <*> CodensityIx cx =
    CodensityIx $ \ k -> cf $ \ f -> cx (k . f)
instance i ~ j => Monad (CodensityIx t i j m) where
  return = pure
  CodensityIx cx >>= k =
    CodensityIx $ \ c -> cx (\ x -> runCodensityIx (k x) c)
instance (IxMonadTrans t, i ~ j) => MonadTrans (CodensityIx t i j) where
  lift m = CodensityIx (\k -> bindIx k (lift m))
instance (i ~ j, Alternative (t i j m), IxMonadTrans t, Monad m)
  => Alternative (CodensityIx t i j m) where
    empty = liftCodensityIx empty
    x <|> y = liftCodensityIx (lowerCodensityIx x <|> lowerCodensityIx y)
instance (i ~ j, Alternative (t i j m), IxMonadTrans t, Monad m)
  => MonadPlus (CodensityIx t i j m)
