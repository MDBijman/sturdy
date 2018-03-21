{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Vals.Concrete.Semantics where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), ($), (.), fst,fmap)
import qualified Prelude

import WhileLanguage (HasStore(..), HasRandomGen(..), Statement, Expr, Label)
import qualified WhileLanguage as L
import Vals.Concrete.Val

import Data.Error
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State

import System.Random

-----------
-- Eval
-----------

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store) => c (Text,Label) Val
lookup = proc (x,l) -> do
  st <- getStore -< ()
  case Map.lookup x st of
    Just v -> returnA -< v
    Nothing -> failA -< "variable not found"

boolLit :: Arrow c => c (Bool,Label) Val
boolLit = arr (\(b,l) -> BoolVal b)

and :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
and = proc (v1,v2,l) -> case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
  _ -> failA -< "Expected two booleans as arguments for 'and'"

or :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
or = proc (v1,v2,l) -> case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
  _ -> failA -< "Expected two booleans as arguments for 'or'"

not :: (ArrowChoice c, ArrowFail String c) => c (Val,Label) Val
not = proc (v,l) -> case v of
  BoolVal b -> returnA -< BoolVal (Prelude.not b)
  _ -> failA -< "Expected a boolean as argument for 'not'"

numLit :: Arrow c => c (Double,Label) Val
numLit = arr (\(d,l) -> NumVal d)

randomNum :: (Arrow c, HasRandomGen c) => c Label Val
randomNum = proc l -> do
  n <- nextRandom -< ()
  returnA -< NumVal n

add :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
add = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
  _ -> failA -< "Expected two numbers as arguments for 'add'"

sub :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
sub = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
  _ -> failA -< "Expected two numbers as arguments for 'sub'"

mul :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
mul = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
  _ -> failA -< "Expected two numbers as arguments for 'mul'"

div :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
div = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
  _ -> failA -< "Expected two numbers as arguments for 'div'"

eq :: (ArrowChoice c, ArrowFail String c) => c (Val,Val,Label) Val
eq = proc (v1,v2,l) -> case (v1,v2) of
  (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
  (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
  _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

fixEval :: Arrow c => (c Expr v -> c Expr v) -> c Expr v
fixEval f = f (fixEval f)



----------
-- Run
----------

store :: (ArrowChoice c, HasStore c Store) => c (Text,Val,Label) ()
store = modifyStore (arr $ \((x,v,_),st) -> Map.insert x v st)

if_ :: (ArrowChoice c, ArrowFail String c) => c [Statement] () -> c [Statement] () -> c (Val,([Statement],[Statement]),Label) ()
if_ f1 f2 = proc (v,(x,y),_) -> case v of
  BoolVal True -> f1 -< x
  BoolVal False -> f2 -< y
  _ -> failA -< "Expected boolean as argument for 'if'"



----------
-- Arrows
----------

type State = (Store,StdGen)
initState :: State
initState = (initStore, mkStdGen 0)

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (Fix (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runFix (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,())
run = fmap (first fst) . runM

runLifted :: [Statement] -> Error String (LiftedStore,())
runLifted = fmap (first liftStore) . run

instance L.HasStore M Store where
  getStore = getA >>> arr fst
  putStore = modifyA $ arr $ \(st,(_,rnd)) -> (st,rnd)

instance L.HasRandomGen M where
  nextRandom = proc () -> do
    (st, gen) <- getA -< ()
    let (r, gen') = random gen
    putA -< (st, gen')
    returnA -< r

instance L.Eval M Val  where
  lookup = lookup
  boolLit = boolLit
  and = and
  or = or
  not = not
  numLit = numLit
  randomNum = randomNum
  add = add
  sub = sub
  mul = mul
  div = div
  eq = eq
  fixEval = fixEval

instance L.Run M Val where
  store = store
  if_ = if_
