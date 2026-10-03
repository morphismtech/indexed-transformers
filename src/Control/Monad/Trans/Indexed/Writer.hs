{- |
Module      :  Control.Monad.Trans.Indexed.Writer
Copyright   :  (C) 2026 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The writer indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.Writer
  ( WriterIx (..)
  , evalWriterIx
  , execWriterIx
  , mapWriterIx
  , tellIx
  , listenIx
  , listensIx
  , passIx
  , censorIx
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans
import Control.Monad.Trans.Indexed

{- | An indexed writer monad parameterized by:

  * @w@ - the output to accumulate.

  * @m@ - The inner monad.

The 'return' function produces the output 'id', while `bindIx`
combines the outputs of the subcomputations using '>>>'.
-}
newtype WriterIx w i j m x = WriterIx {runWriterIx :: m (x, w i j)}
  deriving Functor

instance Category w => IxMonadTrans (WriterIx w) where
  joinIx (WriterIx mm) = WriterIx $ do
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
  return = pure
  (>>=) = flip bindIx
instance (i ~ j, Category w) => MonadTrans (WriterIx w i j) where
  lift m = WriterIx $ do
    x <- m
    return (x, id)

{- | Extract the return value from a writer computation. -}
evalWriterIx :: Monad m => WriterIx w i j m x -> m x
evalWriterIx (WriterIx m) = fst <$> m

{- | Extract the output from a writer computation. -}
execWriterIx :: Monad m => WriterIx w i j m x -> m (w i j)
execWriterIx (WriterIx m) = snd <$> m

{- | Map both the return value and output of a computation using
the given function. -}
mapWriterIx
  :: (m (x, w i j) -> n (y, q i j))
  -> WriterIx w i j m x
  -> WriterIx q i j n y
mapWriterIx f m = WriterIx $ f (runWriterIx m)

{- | @'tellIx' w@ is an action that produces the output @w@. -}
tellIx :: Monad m => w i j -> WriterIx w i j m ()
tellIx w = WriterIx (return ((), w))

{- | @'listenIx' m@ is an action that executes the action @m@ and adds its
output to the value of the computation. -}
listenIx :: Monad m => WriterIx w i j m x -> WriterIx w i j m (x, w i j)
listenIx (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return ((x, w),w)

{- | @'listensIx' f m@ is an action that executes the action @m@ and adds
the result of applying @f@ to the output to the value of the computation. -}
listensIx
  :: Monad m
  => (w i j -> y)
  -> WriterIx w i j m x
  -> WriterIx w i j m (x, y)
listensIx f (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return ((x, f w), w)

{- | @'passIx' m@ is an action that executes the action @m@, which returns
a value and a function, and returns the value, applying the function
to the output. -}
passIx
  :: Monad m
  => WriterIx w i j m (x, w i j -> q i j)
  -> WriterIx q i j m x
passIx (WriterIx m) = WriterIx $ do
  ((x, f), w) <- m
  return (x, f w)

{- | @'censorIx' f m@ is an action that executes the action @m@ and
applies the function @f@ to its output, leaving the return value
unchanged. -}
censorIx :: Monad m => (w i j -> w i j) -> WriterIx w i j m x -> WriterIx w i j m x
censorIx f (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return (x, f w)
