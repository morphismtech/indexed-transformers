{- |
Module      :  Control.Monad.Trans.Indexed.State
Copyright   :  (C) 2026 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The state indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.State
  ( StateIx (..)
  , evalStateIx
  , execStateIx
  , toStateT
  , fromStateT
  , IxMonadTransState (..), modifyIx
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Do qualified as Indexed

{- | An indexed state transformer monad parameterized by:

  * @i@ - The initial state.

  * @j@ - The final state.

  * @m@ - The inner monad.

The 'return' function leaves the state unchanged, while 'bindIx' uses
the final state of the first computation as the initial state of
the second.

An efficient encoding of `StateIx`, up to the retraction
`Control.Monad.Trans.Indexed.Codensity.lowerToStateIx`, is
`Control.Monad.Trans.Indexed.Codensity.PredensityIx` `ReaderT`.
-}
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

{- | Evaluate a state computation with the given initial state
and return the final value, discarding the final state.
-}
evalStateIx :: Monad m => StateIx i j m x -> i -> m x
evalStateIx m i = fst <$> runStateIx m i

{- | Evaluate a state computation with the given initial state
and return the final state, discarding the final value.
-}
execStateIx :: Monad m => StateIx i j m x -> i -> m j
execStateIx m i = snd <$> runStateIx m i

{- | Convert to `StateT`. -}
toStateT :: StateIx i i m x -> StateT i m x
toStateT (StateIx f) = StateT f

{- | Convert from `StateT`. -}
fromStateT :: StateT i m x -> StateIx i i m x
fromStateT (StateT f) = StateIx f

{- | Minimal definition is either both of @getIx@ and @putIx@ or just @stateIx@ -}
class
  ( IxMonadTrans t
  , forall i m. Monad m => MonadState i (t i i m)
  ) => IxMonadTransState t where
  {-# MINIMAL stateIx | getIx, putIx #-}
  -- | Return the state from the internals of the monad.
  getIx :: Monad m => t i i m i
  getIx = stateIx (\i -> return (i,i))
  -- | Replace the state inside the monad.
  putIx :: Monad m => j -> t i j m ()
  putIx i = stateIx (\_ -> return ((),i))
  -- | Embed a state action into the monad.
  stateIx :: Monad m => (i -> m (x,j)) -> t i j m x
  stateIx f = Indexed.do
    i <- getIx
    ~(x, j) <- lift (f i)
    putIx j
    return x
instance IxMonadTransState StateIx where
  stateIx = StateIx

{- | @'modifyIx' f@ is an action that updates the state to the result of
applying @f@ to the current state.

> prop> modifyIx f = getIx & bindIx (putIx . f)
-}
modifyIx :: (IxMonadTransState t, Monad m) => (i -> j) -> t i j m ()
modifyIx f = stateIx (\i -> return ((), f i))
