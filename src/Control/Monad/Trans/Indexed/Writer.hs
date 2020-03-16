{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , PolyKinds
#-}

module Control.Monad.Trans.Indexed.Writer
  ( WriterIx (..)
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Writer

newtype WriterIx w i j m x = WriterIx {runWriterIx :: m (x, w i j)}
  deriving Functor

instance Category w => IndexedMonadTrans (WriterIx w) where
  ixJoin (WriterIx mm) = WriterIx $ do
    (WriterIx m, ij) <- mm
    (x, jk) <- m
    return (x, ij >>> jk)
instance (i ~ j, Applicative m, Category w) => Applicative (WriterIx w i j m) where
  pure x = WriterIx (pure (x, id))
  WriterIx mf <*> WriterIx mx =
    let
      apply (f, ij) (x, jk) = (f x, ij >>> jk)
    in
      WriterIx $ apply <$> mf <*> mx
instance (i ~ j, Monad m, Category w) => Monad (WriterIx w i j m) where
  return x = WriterIx (return (x, id))
  (>>=) = flip ixBind
instance (i ~ j, Category w) => MonadTrans (WriterIx w i j) where
  lift m = WriterIx $ do
    x <- m
    return (x, id)

tellIx :: Monad m => w i j -> WriterIx w i j m ()
tellIx w = WriterIx (return ((), w))
