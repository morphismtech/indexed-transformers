{- |
Module      :  Control.Monad.Trans.Indexed.Cont
Copyright   :  (C) 2026 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The continuation indexed monad transformer.

Delimited continuation operators are taken from Kenichi Asai and Oleg
Kiselyov's tutorial at CW 2011, [Introduction to programming with
shift and reset](http://okmij.org/ftp/continuations/#tutorial).
-}

module Control.Monad.Trans.Indexed.Cont
  ( ContIx (..)
  , callCCIx
  , evalContIx
  , mapContIx
  , withContIx
  , resetIx
  , shiftIx
  , toContT
  , fromContT
  ) where

import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.Trans.Indexed

{- | The continuation indexed monad transformer.
Can be used to add continuation handling to any type constructor:
the 'Monad' instance and most of the operations do not require @m@
to be a monad.
-}
newtype ContIx i j m x = ContIx {runContIx :: (x -> m j) -> m i}
  deriving Functor
instance IxMonadTrans ContIx where
  joinIx (ContIx k) = ContIx $ \f -> k $ \(ContIx g) -> g f
instance i ~ j => Applicative (ContIx i j m) where
  pure x = ContIx $ \k -> k x
  ContIx cf <*> ContIx cx = ContIx $ \ k -> cf $ \ f -> cx (k . f)
instance i ~ j => Monad (ContIx i j m) where
  return = pure
  ContIx cx >>= k = ContIx $ \ c -> cx (\ x -> runContIx (k x) c)
instance i ~ j => MonadTrans (ContIx i j) where
  lift = ContIx . (>>=)
instance i ~ j => MonadCont (ContIx i j m) where callCC = callCCIx

{- | The result of running a CPS computation with 'return' as the
final continuation.

prop> evalContIx (lift m) = m
-}
evalContIx :: Monad m => ContIx x j m j -> m x
evalContIx c = runContIx c return

{- | Apply a function to transform the result of a continuation-passing
computation.

prop> runContIx (mapContIx f m) = f . runContIx m
-}
mapContIx :: (m i -> m j) -> ContIx i k m x -> ContIx j k m x
mapContIx g (ContIx f) = ContIx $ g . f

{- | Apply a function to transform the continuation passed to a CPS
computation.

prop> runContIx (withContIx f m) = runContIx m . f
-}
withContIx :: ((y -> m k) -> x -> m j) -> ContIx i j m x -> ContIx i k m y
withContIx f (ContIx g) = ContIx $ g . f

{- | @callCCIx@ (call-with-current-continuation) calls its argument
function, passing it the current continuation.  It provides
an escape continuation mechanism for use with continuation
monads.  Escape continuations one allow to abort the current
computation and return a value immediately.  They achieve
a similar effect to 'Control.Monad.Trans.Except.throwE'
and 'Control.Monad.Trans.Except.catchE' within an
'Control.Monad.Trans.Except.ExceptT' monad.  The advantage of this
function over calling 'return' is that it makes the continuation
explicit, allowing more flexibility and better control.

The standard idiom used with @callCCIx@ is to provide a lambda-expression
to name the continuation. Then calling the named continuation anywhere
within its scope will escape from the computation, even if it is many
layers deep within nested computations.
-}
callCCIx :: ((x -> ContIx j k m y) -> ContIx i j m x) -> ContIx i j m x
callCCIx f = ContIx $ \k -> runContIx (f (ContIx . const . k)) k

{- | @'shiftIx' f@ captures the continuation up to the nearest enclosing
'resetIx' and passes it to @f@:

prop> resetIx (shiftIx f >>= k) = resetIx (f (evalContIx . k))
-}
shiftIx :: Monad m => ((x -> m j) -> ContIx i k m k) -> ContIx i j m x
shiftIx f = ContIx (evalContIx . f)

{- | @'resetIx' m@ delimits the continuation of any 'shiftIx' inside @m@.

prop> resetIx (lift m) = lift m
-}
resetIx :: Monad m => ContIx x j m j -> ContIx i i m x
resetIx = lift . evalContIx

{- | Convert to `ContT`. -}
toContT :: ContIx i i m x -> ContT i m x
toContT (ContIx f) = ContT f

{- | Convert from `ContT`. -}
fromContT :: ContT i m x -> ContIx i i m x
fromContT (ContT f) = ContIx f
