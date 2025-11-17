{-# LANGUAGE
DerivingStrategies
, GeneralizedNewtypeDeriving
, StandaloneDeriving
, TypeApplications
#-}

module Control.Monad.Trans.Indexed.Codensity
  ( CodensityIx (..)
  , lowerCodensityIx
  , liftCodensityIx
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed

newtype CodensityIx t i j m a = CodensityIx
  { runCodensityIx :: forall b k. (a -> t j k m b) -> t i k m b }
  deriving Functor

lowerCodensityIx
  :: (IxMonadTrans t, Monad m)
  => CodensityIx t i j m a -> t i j m a
lowerCodensityIx (CodensityIx f) = f $ \a -> return a

liftCodensityIx
  :: (IxMonadTrans t, Monad m)
  => t i j m a -> CodensityIx t i j m a
liftCodensityIx m = CodensityIx $ \h -> bindIx h m

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
instance (i ~ j, MonadReader i (t i j m), IxMonadTrans t, Monad m)
  => MonadReader i (CodensityIx t i j m) where
  ask = liftCodensityIx ask
  local f m = liftCodensityIx (local f (lowerCodensityIx m))
instance (i ~ j, MonadReader i (t i j m), IxMonadTrans t, Monad m)
  => MonadState i (CodensityIx t i j m) where
  get = ask
  put s = liftCodensityIx (local (const s) (pure ()))
