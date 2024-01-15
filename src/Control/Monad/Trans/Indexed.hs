module Control.Monad.Trans.Indexed
  ( IndexedMonadTrans (..)
  , Indexed (..)
  ) where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.Trans
import Data.Function ((&))
import Data.Kind
import Prelude hiding (id, (.))

{- | An [Atkey indexed monad]
(https://bentnib.org/paramnotions-jfp.pdf)
is a `Functor` [enriched category]
(https://ncatlab.org/nlab/show/enriched+category).
An indexed monad transformer transforms a `Monad` into an indexed monad,
and is a monad transformer when its source and target are the same,
enabling use of standard @do@ notation for endo-index operations.
-}
type IndexedMonadTrans
  :: (k -> k -> (Type -> Type) -> Type -> Type)
  -> Constraint
class
  ( forall i j m. Monad m => Functor (t i j m)
  , forall i j m. (i ~ j, Monad m) => Monad (t i j m)
  , forall i j. i ~ j => MonadTrans (t i j)
  ) => IndexedMonadTrans t where

  {-# MINIMAL joinIx | bindIx #-}

  -- | indexed analog of `<*>`
  apIx
    :: Monad m
    => t i j m (x -> y)
    -> t j k m x
    -> t i k m y
  apIx tf tx = bindIx (<$> tx) tf

  -- | indexed analog of `join`
  joinIx
    :: Monad m
    => t i j m (t j k m y)
    -> t i k m y
  joinIx t = t & bindIx id

  -- | indexed analog of `=<<`
  bindIx
    :: Monad m
    => (x -> t j k m y)
    -> t i j m x
    -> t i k m y
  bindIx f t = joinIx (f <$> t)

  -- | indexed analog of flipped `>>`
  thenIx
    :: Monad m
    => t j k m y
    -> t i j m x
    -> t i k m y
  thenIx ix2 ix1 = ix1 & bindIx (\ _ -> ix2)

  -- | indexed analog of `<=<`
  andThenIx
    :: Monad m
    => (y -> t j k m z)
    -> (x -> t i j m y)
    -> x -> t i k m z
  andThenIx g f x = bindIx g (f x)

{- | `Indexed` reshuffles the type parameters of an `IndexedMonadTrans`,
exposing its `Category` instance.-}
newtype Indexed t m r i j = Indexed {runIndexed :: t i j m r}
instance
  ( IndexedMonadTrans t
  , Monad m
  , Monoid r
  ) => Category (Indexed t m r) where
    id = Indexed (pure mempty)
    Indexed g . Indexed f = Indexed $ apIx (fmap (<>) f) g
