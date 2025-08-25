{- |
Module      :  Control.Monad.Trans.Indexed.State
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The state indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.State
  ( StateIx (..)
  , evalStateIx
  , execStateIx
  , modifyIx
  , putIx
  , toStateT
  , fromStateT
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Indexed

newtype StateIx i j m x = StateIx { runStateIx :: i -> m (x, j)}
  deriving Functor
instance IxMonadTrans StateIx where
  joinIx (StateIx f) = StateIx $ \i -> do
    (StateIx g, j) <- f i
    g j
instance (i ~ j, Monad m) => Applicative (StateIx i j m) where
  pure x = StateIx $ \i -> pure (x, i)
  (<*>) = apIx
instance (i ~ j, Monad m) => Monad (StateIx i j m) where
  return = pure
  (>>=) = flip bindIx
instance i ~ j => MonadTrans (StateIx i j) where
  lift m = StateIx $ \i -> (, i) <$> m
instance (i ~ j, Monad m) => MonadState i (StateIx i j m) where
  state f = StateIx (return . f)
instance (i ~ j, MonadThrow m)
  => MonadThrow (StateIx i j m) where
    throwM = lift . throwM
instance (i ~ j, MonadCatch m)
  => MonadCatch (StateIx i j m) where
    catch (StateIx m) h = StateIx $ \i ->
      catch (m i) (\e -> runStateIx (h e) i)
instance (i ~ j, MonadMask m)
  => MonadMask (StateIx i j m) where
  mask a = StateIx $ \s -> mask $ \u -> runStateIx (a $ q u) s
    where q u (StateIx b) = StateIx (u . b)
  uninterruptibleMask a =
    StateIx $ \s -> uninterruptibleMask $ \u -> runStateIx (a $ q u) s
      where q u (StateIx b) = StateIx (u . b)
  generalBracket acquire release use = StateIx $ \s0 -> do
    ((b, _s2), (c, s3)) <- generalBracket
      (runStateIx acquire s0)
      (\(resource, s1) exitCase -> case exitCase of
        ExitCaseSuccess (b, s2) -> runStateIx (release resource (ExitCaseSuccess b)) s2
        -- In the two other cases, the base monad overrides @use@'s state
        -- changes and the state reverts to @s1@.
        ExitCaseException e     -> runStateIx (release resource (ExitCaseException e)) s1
        ExitCaseAbort           -> runStateIx (release resource ExitCaseAbort) s1)
      (\(resource, s1) -> runStateIx (use resource) s1)
    return ((b, c), s3)

evalStateIx :: Monad m => StateIx i j m x -> i -> m x
evalStateIx m i = fst <$> runStateIx m i

execStateIx :: Monad m => StateIx i j m x -> i -> m j
execStateIx m i = snd <$> runStateIx m i

modifyIx :: Applicative m => (i -> j) -> StateIx i j m ()
modifyIx f = StateIx $ \i -> pure ((), f i)

putIx :: Applicative m => j -> StateIx i j m ()
putIx j = modifyIx (\ _ -> j)

toStateT :: StateIx i i m x -> StateT i m x
toStateT (StateIx f) = StateT f

fromStateT :: StateT i m x -> StateIx i i m x
fromStateT (StateT f) = StateIx f
