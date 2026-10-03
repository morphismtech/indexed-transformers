# indexed-transformers

An [Atkey indexed monad](https://bentnib.org/paramnotions-jfp.pdf)
is an endo`Functor` [enriched category](https://ncatlab.org/nlab/show/enriched+category),
or [efect](https://mail.haskell.org/pipermail/haskell-cafe/2004-July/006448.html) for short.
An indexed monad transformer transforms a `Monad` into an indexed monad.

This library provides
  - a typeclass for indexed monad transformers
  - qualified do notation to use with them
  - a typeclass for free indexed monad transformers
  - a typeclass for state indexed monad transformers
  - and instances for the
    - free indexed monad transformers
    - codensity indexed monad transformers
    - continuation indexed monad transformer
    - state indexed monad transformer
    - writer indexed monad transformer
