{-|
>>> :set -XQualifiedDo
>>> import qualified Control.Monad.Trans.Indexed.Do as Indexed
-}
module Control.Monad.Trans.Indexed.Do
  ( (>>=)
  , (>>)
  , return
  , fail
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.Indexed as Ix
import Prelude hiding ((>>=), (>>), fail)

(>>=)
  :: (Ix.IndexedMonadTrans t, M.Monad m)
  => t i j m x
  -> (x -> t j k m y)
  -> t i k m y
(>>=) = flip Ix.bindIx

(>>)
  :: (Ix.IndexedMonadTrans t, M.Monad m)
  => t i j m x
  -> t j k m y
  -> t i k m y
(>>) = flip Ix.thenIx

fail
  :: (Ix.IndexedMonadTrans t, M.MonadFail m, i ~ j)
  => String
  -> t i j m x
fail = T.lift . M.fail
