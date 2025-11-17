module Control.Monad.Trans.Indexed.State.Kan
  ( StateIx
  , ReadStx (..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed.Codensity
import Control.Monad.Trans.Indexed.Free
import Control.Monad.Trans.Indexed.Free.Wrap

type StateIx = CodensityIx (FreeIx (Ixer ReadStx))

data ReadStx s t x where AskStx :: ReadStx s s s

instance (s ~ t, Monad m, IxMonadTransFree freeIx)
  => MonadReader s (freeIx (Ixer ReadStx) s t m) where
    ask = liftFreerIx AskStx
    local f m = do
      s <- ask
      lowerCodensityIx $ do
        put (f s)
        x <- liftCodensityIx m
        put s
        return x
