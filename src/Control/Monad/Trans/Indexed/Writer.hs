{- |
Module      :  Control.Monad.Trans.Indexed.Writer
Copyright   :  (C) 2024 Eitan Chatav
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
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Morph
import Control.Monad.Trans.Indexed

newtype WriterIx w i j m x = WriterIx {runWriterIx :: m (x, w i j)}
  deriving Functor

instance Category w => IxMonadTrans (WriterIx w) where
  joinIx (WriterIx mm) = WriterIx $ do
    (WriterIx m, ij) <- mm
    (x, jk) <- m
    return (x, ij >>> jk)
instance (i ~ j, Applicative m, Category w)
  => Applicative (WriterIx w i j m) where
    pure x = WriterIx (pure (x, id))
    WriterIx mf <*> WriterIx mx =
      let
        apply (f, ij) (x, jk) = (f x, ij >>> jk)
      in
        WriterIx $ apply <$> mf <*> mx
instance (i ~ j, Alternative m, Category w)
  => Alternative (WriterIx w i j m) where
    empty = WriterIx empty
    WriterIx mx <|> WriterIx my = WriterIx (mx <|> my)
instance (i ~ j, Monad m, Category w) => Monad (WriterIx w i j m) where
  return = pure
  (>>=) = flip bindIx
instance (i ~ j, MonadPlus m, Category w)
  => MonadPlus (WriterIx w i j m) where
  mzero = WriterIx mzero
  mplus (WriterIx mx) (WriterIx my) = WriterIx (mplus mx my)
instance (i ~ j, Category w) => MonadTrans (WriterIx w i j) where
  lift m = WriterIx $ do
    x <- m
    return (x, id)
instance MFunctor (WriterIx w i j) where
  hoist f (WriterIx m) = WriterIx $ f m
instance (i ~ j, Category w) => MMonad (WriterIx w i j) where
  embed f (WriterIx m) = WriterIx $ do
    ((b,w0),w1) <- runWriterIx $ f m
    return (b, w0 >>> w1)
instance (i ~ j, Category w, MonadThrow m)
  => MonadThrow (WriterIx w i j m) where
    throwM = lift . throwM
instance (i ~ j, Category w, MonadCatch m)
  => MonadCatch (WriterIx w i j m) where
    catch (WriterIx m) h = WriterIx $ catch m (runWriterIx . h)
instance (i ~ j, Category w, MonadMask m)
  => MonadMask (WriterIx w i j m) where
  mask a = WriterIx $ mask $ \u -> runWriterIx (a $ q u)
    where q u b = WriterIx $ u (runWriterIx b)
  uninterruptibleMask a =
    WriterIx $ uninterruptibleMask $ \u -> runWriterIx (a $ q u)
      where q u b = WriterIx $ u (runWriterIx b)
  generalBracket acquire release use = WriterIx $ do
    ((b, _w12), (c, w123)) <- generalBracket
      (runWriterIx acquire)
      (\(resource, w1) exitCase -> case exitCase of
        ExitCaseSuccess (b, w12) -> do
          (c, w3) <- runWriterIx (release resource (ExitCaseSuccess b))
          return (c, w12 >>> w3)
        -- In the two other cases, the base monad overrides @use@'s state
        -- changes and the state reverts to @w1@.
        ExitCaseException e -> do
          (c, w3) <- runWriterIx (release resource (ExitCaseException e))
          return (c, w1 >>> w3)
        ExitCaseAbort -> do
          (c, w3) <- runWriterIx (release resource ExitCaseAbort)
          return (c, w1 >>> w3))
      (\(resource, w1) -> do
        (a, w2) <- runWriterIx (use resource)
        return (a, w1 >>> w2))
    return ((b, c), w123)

evalWriterIx :: Monad m => WriterIx w i j m x -> m x
evalWriterIx (WriterIx m) = fst <$> m

execWriterIx :: Monad m => WriterIx w i j m x -> m (w i j)
execWriterIx (WriterIx m) = snd <$> m

mapWriterIx
  :: (m (x, w i j) -> n (y, q i j))
  -> WriterIx w i j m x
  -> WriterIx q i j n y
mapWriterIx f m = WriterIx $ f (runWriterIx m)

tellIx :: Monad m => w i j -> WriterIx w i j m ()
tellIx w = WriterIx (return ((), w))

listenIx :: Monad m => WriterIx w i j m x -> WriterIx w i j m (x, w i j)
listenIx (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return ((x, w),w)

listensIx
  :: Monad m
  => (w i j -> y)
  -> WriterIx w i j m x
  -> WriterIx w i j m (x, y)
listensIx f (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return ((x, f w), w)

passIx
  :: Monad m
  => WriterIx w i j m (x, w i j -> q i j)
  -> WriterIx q i j m x
passIx (WriterIx m) = WriterIx $ do
  ((x, f), w) <- m
  return (x, f w)

censorIx :: Monad m => (w i j -> w i j) -> WriterIx w i j m x -> WriterIx w i j m x
censorIx f (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return (x, f w)
