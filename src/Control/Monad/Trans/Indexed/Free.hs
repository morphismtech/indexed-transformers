{-# LANGUAGE
    ConstraintKinds
  , DefaultSignatures
  , GADTs
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , UndecidableInstances
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Control.Monad.Trans.Indexed.Free
  ( IxFree
  , coerceIxFree
  ) where

import Control.Monad.Free
import Control.Monad.Trans.Indexed
import Data.Silo
import Data.Silo.Functor

class
  ( SFoldable free
  , SMonad free
  , forall g. Silo g => IndexedMonadTrans (free g)
  , forall g m i j. (Silo g, Monad m, i ~ j) => MonadFree (g i j) (free g i j m)
  ) => IxFree free  where

coerceIxFree
  :: (IxFree free0, IxFree free1, Silo g, Monad m)
  => free0 g i j m x -> free1 g i j m x 
coerceIxFree = sfoldMap slift
