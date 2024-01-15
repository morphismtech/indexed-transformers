{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{- |
Module      :  Control.Monad.Trans.Indexed.Free
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The free indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.Free
  ( IxMonadTransFree (liftFreeIx, hoistFreeIx, foldFreeIx), coerceFreeIx
  , IxFunctor, IxMap (IxMap), liftFreerIx, hoistFreerIx, foldFreerIx
  ) where

import Control.Monad.Free
import Control.Monad.Trans.Indexed
import Data.Kind

{- |
The free `IxMonadTrans` generated by an `IxFunctor`
is characterized by the `IxMonadTransFree` class
up to the isomorphism `coerceFreeIx`.

`IxMonadTransFree` and `IxMap`, the free `IxMonadTrans` and
the free `IxFunctor`, can be combined as a "freer" `IxMonadTrans`
and used as a DSL generated by primitive commands like this
[Conor McBride example]
(https://stackoverflow.com/questions/28690448/what-is-indexed-monad).

>>> :set -XGADTs -XDataKinds
>>> import Data.Kind
>>> type DVD = String
>>> :{
data DVDCommand
  :: Bool -- ^ drive is full before command
  -> Bool -- ^ drive is full after command
  -> Type -- ^ return type
  -> Type where
  Insert :: DVD -> DVDCommand 'False 'True ()
  Eject :: DVDCommand 'True 'False DVD
:}

>>> :{
insert
  :: (IxMonadTransFree freeIx, Monad m)
  => DVD -> freeIx (IxMap DVDCommand) 'False 'True m ()
insert dvd = liftFreerIx (Insert dvd)
:}

>>> :{
eject
  :: (IxMonadTransFree freeIx, Monad m)
  => freeIx (IxMap DVDCommand) 'True 'False m DVD
eject = liftFreerIx Eject
:}

>>> :set -XQualifiedDo
>>> import qualified Control.Monad.Trans.Indexed.Do as Indexed
>>> :{
swap
  :: (IxMonadTransFree freeIx, Monad m)
  => DVD -> freeIx (IxMap DVDCommand) 'True 'True m DVD
swap dvd = Indexed.do
  dvd' <- eject
  insert dvd
  return dvd'
:}

>>> import Control.Monad.Trans
>>> :{
printDVD :: IxMonadTransFree freeIx => freeIx (IxMap DVDCommand) 'True 'True IO ()
printDVD = Indexed.do
  dvd <- eject
  insert dvd
  lift $ putStrLn dvd
:}

-}
class
  ( forall f. IxFunctor f => IxMonadTrans (freeIx f)
  , forall f m i j. (IxFunctor f, Monad m, i ~ j)
    => MonadFree (f i j) (freeIx f i j m)
  ) => IxMonadTransFree freeIx where
  liftFreeIx
    :: (IxFunctor f, Monad m)
    => f i j x
    -> freeIx f i j m x
  hoistFreeIx
    :: (IxFunctor f, IxFunctor g, Monad m)
    => (forall i j x. f i j x -> g i j x)
    -> freeIx f i j m x -> freeIx g i j m x
  foldFreeIx
    :: (IxFunctor f, IxMonadTrans t, Monad m)
    => (forall i j x. f i j x -> t i j m x)
    -> freeIx f i j m x -> t i j m x

{- |
prop> coerceFreeIx = foldFreeIx liftFreeIx
prop> id = coerceFreeIx . coerceFreeIx
-}
coerceFreeIx
  :: (IxMonadTransFree freeIx0, IxMonadTransFree freeIx1, IxFunctor f, Monad m)
  => freeIx0 f i j m x -> freeIx1 f i j m x 
coerceFreeIx = foldFreeIx liftFreeIx

type IxFunctor
  :: (k -> k -> Type -> Type)
  -> Constraint
type IxFunctor f = forall i j. Functor (f i j)

{- |
`IxMap` is the free `IxFunctor`. It's a left Kan extension.
Combining `IxMonadTransFree` with `IxMap` as demonstrated in the above example,
gives the "freer" `IxMonadTrans`, modeled on this
[Oleg Kiselyov explanation]
(https://okmij.org/ftp/Computation/free-monad.html#freer).
-}
data IxMap f i j x where
  IxMap :: (x -> y) -> f i j x -> IxMap f i j y
instance Functor (IxMap f i j) where
  fmap g (IxMap f x) = IxMap (g . f) x

liftFreerIx
  :: (IxMonadTransFree freeIx, Monad m)
  => f i j x -> freeIx (IxMap f) i j m x
liftFreerIx x = liftFreeIx (IxMap id x)

hoistFreerIx
  :: (IxMonadTransFree freeIx, Monad m)
  => (forall i j x. f i j x -> g i j x)
  -> freeIx (IxMap f) i j m x -> freeIx (IxMap g) i j m x
hoistFreerIx f = hoistFreeIx (\(IxMap g x) -> IxMap g (f x))

foldFreerIx
  :: (IxMonadTransFree freeIx, IxMonadTrans t, Monad m)
  => (forall i j x. f i j x -> t i j m x)
  -> freeIx (IxMap f) i j m x -> t i j m x
foldFreerIx f x = foldFreeIx (\(IxMap g y) -> g <$> f y) x
