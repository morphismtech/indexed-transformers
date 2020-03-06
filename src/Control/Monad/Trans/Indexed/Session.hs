{-# LANGUAGE
    DeriveFunctor
  , GADTs
  , PolyKinds
#-}

module Control.Monad.Trans.Indexed.Session
  ( SessionIx (..)
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Indexed

newtype SessionIx i j m x = SessionIx {runSessionIx :: m x}
  deriving Functor

instance IndexedMonadTrans SessionIx where
  ixJoin (SessionIx m) = SessionIx $ do
    SessionIx n <- m
    n
instance (i ~ j, Applicative m) => Applicative (SessionIx i j m) where
  pure = SessionIx . pure
  SessionIx mf <*> SessionIx mx = SessionIx $ mf <*> mx
instance (i ~ j, Monad m) => Monad (SessionIx i j m) where
  return = SessionIx . return
  (>>=) = flip ixBind
instance i ~ j => MonadTrans (SessionIx i j) where
  lift = SessionIx
