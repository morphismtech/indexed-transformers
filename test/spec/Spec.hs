{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Category (Category, (>>>))
import Control.Category qualified as Category
import Control.Monad
import Control.Monad.Codensity (lowerCodensity)
import Control.Monad.Cont (runContT)
import Control.Monad.Free (wrap)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Codensity
import Control.Monad.Trans.Indexed.Cont
import Control.Monad.Trans.Indexed.Do qualified as Indexed
import Control.Monad.Trans.Indexed.Free
import Control.Monad.Trans.Indexed.State
import Control.Monad.Trans.Indexed.Writer
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Kind
import Data.Proxy
import Test.Hspec
import Test.QuickCheck

type W = Writer [Int]

w :: ([Int], x) -> W x
w (xs, x) = tell xs >> return x

data Prog
  = Done Int
  | Tell Int Prog
  | Op Int
  | Bind Prog (Fun Int Prog)
  deriving Show

instance Arbitrary Prog where
  arbitrary = sized go
    where
      go 0 = oneof [Done <$> arbitrary, Op <$> arbitrary]
      go n = oneof
        [ Done <$> arbitrary
        , Op <$> arbitrary
        , Tell <$> arbitrary <*> go (n - 1)
        , Bind <$> go (n `div` 2) <*> resize (n `div` 2) arbitrary
        ]
  shrink = \case
    Done _ -> []
    Op n -> [Done n]
    Tell _ p -> [p]
    Bind p f -> [p] ++ [Bind p' f | p' <- shrink p]

interp :: IxMonadTrans t => (Int -> t Int Int W Int) -> Prog -> t Int Int W Int
interp op = \case
  Done n -> pure n
  Tell n p -> thenIx (interp op p) (lift (tell [n]))
  Op n -> op n
  Bind p f -> bindIx (interp op . applyFun f) (interp op p)

stateOp :: IxMonadTransState t => Int -> t Int Int W Int
stateOp n = stateIx $ \s -> tell [s] >> return (s, s + n)

observeState :: StateIx Int Int W Int -> Int -> [Int]
observeState m s = let ((x, s'), xs) = runWriter (runStateIx m s) in xs ++ [x, s']

type Cmd :: Type -> Type -> Type -> Type
data Cmd i j x where
  Cmd :: Int -> Cmd Int Int Int

runCmd :: Cmd i j x -> StateIx i j W x
runCmd (Cmd n) = stateOp n

newtype Log i j = Log [Int]
  deriving (Eq, Show)
instance Category Log where
  id = Log []
  Log ys . Log xs = Log (xs ++ ys)

data Subject = forall t. IxMonadTrans t
  => Subject String (Int -> t Int Int W Int) (t Int Int W Int -> Int -> [Int])

data FreeSubject = forall (freeIx :: (Type -> Type -> Type -> Type) -> Type -> Type -> (Type -> Type) -> Type -> Type).
  IxMonadTransFree freeIx => FreeSubject String (Proxy freeIx)

freeSubjects :: [FreeSubject]
freeSubjects =
  [ FreeSubject "FreeIx" (Proxy @FreeIx)
  , FreeSubject "FoldFreeIx" (Proxy @FoldFreeIx)
  , FreeSubject "ImproveFreeIx" (Proxy @ImproveFreeIx)
  ]

freeProg :: IxMonadTransFree freeIx => Prog -> FreerIx freeIx Cmd Int Int W Int
freeProg = interp (liftFreerIx . Cmd)

observeFree :: IxMonadTransFree freeIx => FreerIx freeIx Cmd Int Int W Int -> Int -> [Int]
observeFree = observeState . foldFreerIx runCmd

stateSubjects :: [Subject]
stateSubjects =
  [ Subject "StateIx" stateOp observeState
  , Subject "PredensityIx ReaderT" stateOp (observeState . lowerToStateIx)
  , Subject "CodensityIx StateIx"
      (liftCodensityIx . stateOp) (observeState . lowerCodensityIx)
  , Subject "CodensityIx FreeIx"
      (liftCodensityIx . liftFreerIx . Cmd)
      (observeFree @FreeIx . lowerCodensityIx)
  ] ++
  [ Subject name (liftFreerIx . Cmd) (observeFree @freeIx)
  | FreeSubject name (_ :: Proxy freeIx) <- freeSubjects
  ]

otherSubjects :: [Subject]
otherSubjects =
  [ Subject "WriterIx"
      (\n -> thenIx (pure (n + 1)) (tellIx (Log [n])))
      (\m _ ->
        let ((x, Log ys), xs) = runWriter (runWriterIx m)
        in [length xs] ++ xs ++ ys ++ [x])
  , Subject "ContIx"
      (\n -> ContIx $ \k -> (+ n) <$> k n)
      (\m s ->
        let (x, xs) = runWriter (runContIx m (\y -> tell [y] >> return (2 * y + s)))
        in xs ++ [x])
  ]

laws :: Subject -> Spec
laws (Subject name op run) = describe name $ do
  let
    p = interp op
    (m0 =~= m1) s = run m0 s === run m1 s
  it "left identity" $ property $ \(x :: Int) (Fn f) ->
    bindIx (p . f) (pure x) =~= p (f x)
  it "right identity" $ property $ \m ->
    bindIx pure (p m) =~= p m
  it "associativity" $ property $ \m (Fn f) (Fn g) ->
    bindIx (p . g) (bindIx (p . f) (p m))
      =~= bindIx (andThenIx (p . g) (p . f)) (p m)
  it "joinIx" $ property $ \m (Fn f) ->
    joinIx (p . f <$> p m) =~= bindIx (p . f) (p m)
  it "(>>=) = flip bindIx" $ property $ \m (Fn f) ->
    (p m >>= p . f) =~= bindIx (p . f) (p m)
  it "(<*>) = apIx" $ property $ \m0 m1 (Fn2 f) ->
    (f <$> p m0 <*> p m1) =~= apIx (f <$> p m0) (p m1)
  it "(>>) = flip thenIx" $ property $ \m0 m1 ->
    (p m0 >> p m1) =~= thenIx (p m1) (p m0)
  it "fmap" $ property $ \m (Fn (f :: Int -> Int)) ->
    (f <$> p m) =~= bindIx (pure . f) (p m)
  it "lift . return = return" $ property $ \(x :: Int) ->
    lift (return x) =~= pure x
  it "lift (m >>= f) = lift m >>= lift . f" $ property $ \(m :: ([Int], Int)) (Fn (f :: Int -> ([Int], Int))) ->
    lift (w m >>= w . f) =~= (lift (w m) >>= lift . w . f)

stateLaws
  :: forall t. IxMonadTransState t
  => String
  -> (forall i j x. t i j W x -> StateIx i j W x)
  -> Spec
stateLaws name toState = describe name $ do
  let
    run :: t i j W x -> i -> ((x, j), [Int])
    run m s = runWriter (runStateIx (toState m) s)
  it "getIx" $ property $ \(s :: Int) ->
    run getIx s === ((s, s), [])
  it "putIx" $ property $ \(s :: Int) (s' :: String) ->
    run (putIx s') s === (((), s'), [])
  it "stateIx" $ property $ \(Fn (f :: Int -> ([Int], (Bool, String)))) s ->
    run (stateIx (w . f)) s === runWriter (w (f s))
  it "modifyIx" $ property $ \(Fn (f :: Int -> String)) s ->
    run (modifyIx f) s === (((), f s), [])
  it "get then put" $ property $ \(s :: Int) ->
    run (bindIx putIx getIx) s === run (pure ()) s
  it "put then get" $ property $ \(s :: Int) (s' :: String) ->
    run (thenIx getIx (putIx s')) s === run (thenIx (pure s') (putIx s')) s
  it "put then put" $ property $ \(s :: Int) (s' :: Bool) (s'' :: String) ->
    run (thenIx (putIx s'') (putIx s')) s === run (putIx s'') s
  it "MonadState" $ property $ \(Fn (f :: Int -> Int)) s ->
    run (state (\x -> (x, f x))) s === ((s, f s), [])
  it "Indexed.do" $ property $ \(s :: Int) (s' :: String) ->
    run
      ( Indexed.do
          putIx s'
          x <- getIx
          putIx (length x)
          return x
      ) s
      === ((s', length s'), [])

predensitySpec :: Spec
predensitySpec = describe "PredensityIx ReaderT" $ do
  it "lowerToStateIx . liftFromStateIx = id" $ property $
    \(Fn (f :: Int -> ([Int], (Bool, String)))) s ->
      let m = StateIx (w . f)
      in runWriter (runStateIx (lowerToStateIx (liftFromStateIx m)) s)
        === runWriter (runStateIx m s)
  it "liftFromStateIx . lowerToStateIx = id on stateIx terms" $ property $
    \prog (Fn2 (k :: Int -> Int -> ([Int], Int))) s ->
      let
        m = interp stateOp prog
        run m' = runWriter (runReaderT (runPredensityIx m' (\x -> ReaderT (w . k x))) s)
      in run (liftFromStateIx (lowerToStateIx m)) === run m

codensitySpec :: Spec
codensitySpec = describe "CodensityIx StateIx" $ do
  let p = interp (liftCodensityIx . stateOp)
  it "lowerCodensityIx . liftCodensityIx = id" $ property $ \prog s ->
    let m = interp stateOp prog
    in observeState (lowerCodensityIx (liftCodensityIx m)) s === observeState m s
  it "resetCodensityIx" $ property $ \prog s ->
    observeState (lowerCodensityIx (resetCodensityIx (p prog))) s
      === observeState (lowerCodensityIx (p prog)) s
  it "toCodensity" $ property $ \prog s ->
    observeState (lowerCodensity (toCodensity (p prog))) s
      === observeState (lowerCodensityIx (p prog)) s
  it "wrapCodensityIx" $ property $ \(s :: Int) (s' :: String) ->
    let m0 = lowerCodensityIx (wrapCodensityIx (\m -> thenIx m (putIx s'))) :: StateIx Int String W ()
    in runWriter (runStateIx m0 s) === runWriter (runStateIx (putIx s') s)
  it "shiftCodensityIx" $ property $ \(x :: Int) (n :: Int) (Fn f) s ->
    observeState
      (lowerCodensityIx
        (shiftCodensityIx (\k -> liftCodensityIx (thenIx (k x) (modifyIx (+ n)))) >>= p . f))
      s
      === observeState (thenIx (lowerCodensityIx (p (f x))) (modifyIx (+ n))) s

freeSpec :: FreeSubject -> Spec
freeSpec (FreeSubject name (_ :: Proxy freeIx)) = describe name $ do
  forM_ freeSubjects $ \(FreeSubject name' (_ :: Proxy freeIx')) ->
    it ("coerceFreeIx to " <> name') $ property $ \prog s ->
      observeFree @freeIx' (coerceFreeIx (freeProg @freeIx prog)) s
        === observeFree @freeIx (freeProg prog) s
  it "foldFreeIx f . liftFreeIx = f" $ property $ \n s ->
    observeFree @freeIx (liftFreerIx (Cmd n)) s === observeState (runCmd (Cmd n)) s
  it "hoistFreerIx" $ property $ \prog s ->
    let double :: Cmd i j x -> Cmd i j x
        double (Cmd n) = Cmd (2 * n)
    in observeFree (hoistFreerIx double (freeProg @freeIx prog)) s
      === observeState (foldFreerIx (runCmd . double) (freeProg @freeIx prog)) s
  it "wrap" $ property $ \n (Fn f) s ->
    observeFree @freeIx (wrap (CoyonedaIx (freeProg . f) (Cmd n))) s
      === observeFree @freeIx (bindIx (freeProg . f) (liftFreerIx (Cmd n))) s
  it "improveIx" $ property $ \prog s ->
    observeFree (improveIx (freeProg prog)) s === observeFree @freeIx (freeProg prog) s

contSpec :: Spec
contSpec = describe "ContIx" $ do
  let
    p :: Prog -> ContIx Int Int W Int
    p = interp (\n -> ContIx $ \k -> (+ n) <$> k n)
    eval = runWriter . evalContIx
  it "callCCIx" $ property $ \prog (x :: Int) ->
    eval (callCCIx (\k -> thenIx (p prog) (k x))) === eval (pure x)
  it "shiftIx" $ property $ \(x :: Int) (Fn g) ->
    eval (g <$> shiftIx (\k -> lift (k x >>= k))) === eval (pure (g (g x)))
  it "resetIx" $ property $ \prog ->
    eval (resetIx (p prog) :: ContIx Int Int W Int) === eval (p prog)
  it "withContIx" $ property $ \prog (Fn (f :: Int -> Int)) ->
    eval (withContIx (\k -> k . f) (p prog)) === eval (f <$> p prog)
  it "toContT . fromContT" $ property $ \prog ->
    runWriter (runContT (toContT (fromContT (toContT (p prog)))) return)
      === eval (p prog)

writerSpec :: Spec
writerSpec = describe "WriterIx" $ do
  let
    run :: WriterIx Log i j W x -> ((x, Log i j), [Int])
    run = runWriter . runWriterIx
  it "tellIx" $ property $ \xs ys ->
    run (thenIx (tellIx (Log ys)) (tellIx (Log xs)) :: WriterIx Log Int Bool W ())
      === run (tellIx (Log xs >>> Log ys))
  it "listenIx" $ property $ \xs (x :: Int) ->
    let m = thenIx (pure x) (tellIx (Log xs)) :: WriterIx Log Int Bool W Int
    in run (listenIx m) === (((x, Log xs), Log xs), [])
  it "listensIx" $ property $ \xs (x :: Int) ->
    let m = thenIx (pure x) (tellIx (Log xs)) :: WriterIx Log Int Bool W Int
    in run (listensIx (\(Log ys) -> sum ys) m) === (((x, sum xs), Log xs), [])
  it "censorIx" $ property $ \xs (Fn f) ->
    run (censorIx (\(Log ys) -> Log (f ys)) (tellIx (Log xs) :: WriterIx Log Int Bool W ()))
      === (((), Log (f xs)), [])
  it "passIx" $ property $ \xs (Fn f) (x :: Int) ->
    run (passIx (WriterIx (return ((x, \(Log ys) -> Log (f ys)), Log xs))) :: WriterIx Log Int Bool W Int)
      === ((x, Log (f xs)), [])
  it "evalWriterIx and execWriterIx" $ property $ \xs (x :: Int) ->
    let m = thenIx (pure x) (tellIx (Log xs)) :: WriterIx Log Int Bool W Int
    in (runWriter (evalWriterIx m), runWriter (execWriterIx m)) === ((x, []), (Log xs, []))

categorySpec :: Spec
categorySpec = describe "Indexed StateIx" $ do
  let
    st :: Fun i ([Int], ([Int], j)) -> Indexed StateIx W [Int] i j
    st (Fn f) = Indexed (StateIx (w . f))
    run :: Indexed StateIx W [Int] i j -> i -> (([Int], j), [Int])
    run m s = runWriter (runStateIx (runIndexed m) s)
  it "left identity" $ property $ \(f :: Fun Int ([Int], ([Int], String))) s ->
    run (Category.id Category.. st f) s === run (st f) s
  it "right identity" $ property $ \(f :: Fun Int ([Int], ([Int], String))) s ->
    run (st f Category.. Category.id) s === run (st f) s
  it "associativity" $ property $
    \(f :: Fun Int ([Int], ([Int], String)))
     (g :: Fun String ([Int], ([Int], Bool)))
     (h :: Fun Bool ([Int], ([Int], Int)))
     s ->
      run ((st h Category.. st g) Category.. st f) s
        === run (st h Category.. (st g Category.. st f)) s

stateSpec :: Spec
stateSpec = describe "StateIx" $ do
  it "toStateT" $ property $ \prog s ->
    let m = interp stateOp prog
    in runWriter (runStateT (toStateT m) s) === runWriter (runStateIx m s)
  it "fromStateT . toStateT = id" $ property $ \prog s ->
    let m = interp stateOp prog
    in observeState (fromStateT (toStateT m)) s === observeState m s
  it "evalStateIx and execStateIx" $ property $ \prog s ->
    let m = interp stateOp prog
        ((x, s'), xs) = runWriter (runStateIx m s)
    in (runWriter (evalStateIx m s), runWriter (execStateIx m s)) === ((x, xs), (s', xs))

main :: IO ()
main = hspec $ do
  describe "IxMonadTrans laws" $
    mapM_ laws (stateSubjects ++ otherSubjects)
  describe "state transformers agree with StateIx" $
    forM_ stateSubjects $ \(Subject name op run) ->
      it name $ property $ \prog s ->
        run (interp op prog) s === observeState (interp stateOp prog) s
  describe "IxMonadTransState laws" $ do
    stateLaws "StateIx" id
    stateLaws "PredensityIx ReaderT" lowerToStateIx
  predensitySpec
  codensitySpec
  describe "IxMonadTransFree" $ traverse_ freeSpec freeSubjects
  contSpec
  writerSpec
  categorySpec
  stateSpec
