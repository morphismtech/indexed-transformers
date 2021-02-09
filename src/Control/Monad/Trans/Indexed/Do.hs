{-# LANGUAGE
    GADTs
  , NoImplicitPrelude
#-}

module Control.Monad.Trans.Indexed.Do
  ( (>>=)
  , (>>)
  , fail
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.Indexed as Ix
import qualified Prelude as P

(>>=)
  :: (Ix.IndexedMonadTrans t, M.Monad m)
  => t i j m x
  -> (x -> t j k m y)
  -> t i k m y
(>>=) = P.flip Ix.ixBind

(>>)
  :: (Ix.IndexedMonadTrans t, M.Monad m)
  => t i j m x
  -> t j k m y
  -> t i k m y
(>>) = P.flip Ix.ixThen

fail
  :: (Ix.IndexedMonadTrans t, M.MonadFail m, i ~ j)
  => P.String
  -> t i j m x
fail = T.lift P.. M.fail
