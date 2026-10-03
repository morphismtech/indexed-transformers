module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XConstraintKinds"
  , "-XDeriveFunctor"
  , "-XDerivingStrategies"
  , "-XFlexibleInstances"
  , "-XGADTs"
  , "-XGeneralizedNewtypeDeriving"
  , "-XImportQualifiedPost"
  , "-XLambdaCase"
  , "-XMultiParamTypeClasses"
  , "-XPolyKinds"
  , "-XQualifiedDo"
  , "-XQuantifiedConstraints"
  , "-XRankNTypes"
  , "-XStandaloneDeriving"
  , "-XStandaloneKindSignatures"
  , "-XTupleSections"
  , "-XTypeOperators"
  , "-XUndecidableInstances"
  , "src/Control/Monad/Trans/Indexed/Free.hs"
  ]
