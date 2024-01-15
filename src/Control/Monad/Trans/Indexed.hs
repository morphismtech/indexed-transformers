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

  {-# MINIMAL ixJoin | ixBind #-}

  -- | indexed analog of `<*>`
  ixAp
    :: Monad m
    => t i j m (x -> y)
    -> t j k m x
    -> t i k m y
  ixAp tf tx = ixBind (<$> tx) tf

  -- | indexed analog of `join`
  ixJoin
    :: Monad m
    => t i j m (t j k m y)
    -> t i k m y
  ixJoin t = t & ixBind id

  -- | indexed analog of `=<<`
  ixBind
    :: Monad m
    => (x -> t j k m y)
    -> t i j m x
    -> t i k m y
  ixBind f t = ixJoin (f <$> t)

  -- | indexed analog of flipped `>>`
  ixThen
    :: Monad m
    => t j k m y
    -> t i j m x
    -> t i k m y
  ixThen ix2 ix1 = ix1 & ixBind (\ _ -> ix2)

  -- | indexed analog of `<=<`
  ixAndThen
    :: Monad m
    => (y -> t j k m z)
    -> (x -> t i j m y)
    -> x -> t i k m z
  ixAndThen g f x = ixBind g (f x)

{- | `Indexed` reshuffles the type parameters of an `IndexedMonadTrans`,
exposing its `Category` instance.-}
newtype Indexed t m r i j = Indexed {runIndexed :: t i j m r}
instance
  ( IndexedMonadTrans t
  , Monad m
  , Monoid r
  ) => Category (Indexed t m r) where
    id = Indexed (pure mempty)
    Indexed g . Indexed f = Indexed $ ixAp (fmap (<>) f) g
