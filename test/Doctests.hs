module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XConstraintKinds"
  , "-XDeriveFunctor"
  , "-XFlexibleInstances"
  , "-XGADTs"
  , "-XLambdaCase"
  , "-XMultiParamTypeClasses"
  , "-XPolyKinds"
  , "-XQuantifiedConstraints"
  , "-XRankNTypes"
  , "-XStandaloneKindSignatures"
  , "-XTupleSections"
  , "-XTypeOperators"
  , "src/Control/Monad/Trans/Indexed/Free.hs"
  ]
