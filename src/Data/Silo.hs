{-# LANGUAGE
    ConstraintKinds
  , GADTs
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Silo
  ( Silo
  , Lance (..)
  , mapLance
  , liftLance
  , lowerLance
  ) where

import Data.Kind

{- |
A higher quiver is is a higher kinded type,

@g :: k -> k -> Type -> Type@

  * where vertices are types @i :: k@,
  * and arrows from source @i@ to target @j@
    labeled by datatype @x@ are terms @g i j x@.

`Silo` is a quantified constraint on a higher quiver,
capturing that it is a `Functor` for any source and target.

`Silo`s can be a DSL describing primitive commands like
[this Conor McBride example]
(https://stackoverflow.com/questions/28690448/what-is-indexed-monad).

>>> type DVD = String
>>> :{
data DVDCommand
  :: Bool -- ^ is drive full initially?
  -> Bool -- ^ is drive full finally?
  -> Type -- ^ return type
  -> Type where
  Insert
    :: DVD -- ^ a DVD to insert
    -> x -- ^ then what to do
    -> DVDCommand 'False 'True x -- ^ empty to full
  Eject
    :: (DVD -> x) -- ^ what to do with an ejected DVD
    -> DVDCommand 'True 'False x -- ^ full to empty
:}
>>> deriving instance Functor (DVDCommand i j)
>>> () :: Silo DVDCommand => ()
()
-}
type Silo (g :: k -> k -> Type -> Type) = forall i j. Functor (g i j)

{- | `Lance` is a left Kan extension,
it is the free `Silo` on a higher quiver.

>>> type DVD = String
>>> :{
data DVDCommand
  :: Bool -- ^ is drive full initially?
  -> Bool -- ^ is drive full finally?
  -> Type -- ^ return type
  -> Type where
  Insert :: DVD -> DVDCommand 'False 'True ()
  Eject :: DVDCommand 'True 'False DVD
:}

@DVDCommand@ expresses exactly the signature we want to describe our commands.
Unfortunately, @DVDCommand@ cannot be a `Silo`,
it's just not polymorphic enough.
@Lance DVDCommand@ however _is_ a `Silo`,
isomorphic to the previous definition of @DVDCommand@.

>>> () :: Silo (Lance DVDCommand) => ()
()
>>> let insert dvd x = liftLance (Insert dvd) $> x
>>> :type insert
insert :: DVD -> x -> Lance DVDCommand 'False 'True x
>>> let eject k = k <$> liftLance Eject
>>> :type eject
eject :: (DVD -> x) -> Data.Silo.Lance DVDCommand 'True 'False x

`Lance` is a free functor,
meaning it's left adjoint to forgetting the `Silo` constraint.
-}
data Lance f i j x where
  Lance :: (x -> y) -> f i j x -> Lance f i j y
instance Functor (Lance f i j) where
  fmap g (Lance f x) = Lance (g . f) x
mapLance :: (forall x. f i j x -> g i j x) -> Lance f i j x -> Lance g i j x
mapLance g (Lance f x) = Lance f (g x)
liftLance :: g i j x -> Lance g i j x
liftLance = Lance id
lowerLance :: Functor (g i j) => Lance g i j x -> g i j x
lowerLance (Lance f x) = fmap f x
