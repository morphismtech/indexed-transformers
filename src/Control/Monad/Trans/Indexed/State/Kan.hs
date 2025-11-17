module Control.Monad.Trans.Indexed.State.Kan
  ( StateIx
  , ReadStx (..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed.Codensity
import Control.Monad.Trans.Indexed.Free
import Control.Monad.Trans.Indexed.Free.Wrap

type StateIx = CodensityIx (FreeIx ReadStx)

data ReadStx s t x where
  NoOpStx :: ReadStx s s ()
  AskStx :: ReadStx s s s
  LocalStx
    :: (u -> s) -> (x -> y)
    -> ReadStx s t x -> ReadStx u t y
instance Functor (ReadStx s t) where
  fmap = LocalStx id

instance (s ~ t, Monad m, IxMonadTransFree freeIx)
  => MonadReader s (freeIx ReadStx s t m) where
    ask = liftFreeIx AskStx
    local f m = lowerCodensityIx $ do
      s <- ask
      put (f s)
      x <- liftCodensityIx m
      put s
      return x
