# indexed-transformers

An [Atkey indexed monad](https://bentnib.org/paramnotions-jfp.pdf)
is a `Functor` [enriched category](https://ncatlab.org/nlab/show/enriched+category).
An indexed monad transformer transforms a `Monad` into an indexed monad.
See also Chung-chieh Shan's clear [explanation](https://mail.haskell.org/pipermail/haskell-cafe/2004-July/006448.html)
of indexed monads which he calls "efects", short for endofunctor-enriched categories.

`IxMonadTrans` is useful as a composable, effectful control structure with statically defined
transition types demarking _before_ and _after_ running the effect.
