{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ValueSemantics.Concrete where

import           Prelude
import qualified Prelude as P

import           Syntax
import           SharedSemantics hiding (run)
import qualified SharedSemantics as Shared

import           Data.Concrete.Error
import qualified Data.Concrete.Store as S
import           Data.Concrete.Store (Store)
import           Data.Hashable
import           Data.Text (Text)
import           Data.Label

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.State
import           Control.Arrow.Fix
import           Control.Arrow.Store
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.Concrete.FixPoint(runFixPoint)

import           System.Random

import           GHC.Generics (Generic)

data Val = BoolVal Bool | NumVal Int deriving (Eq, Show, Generic)
newtype Interp c x y = Interp (State StdGen (StoreArrow Text Val (Except String c)) x y)
type instance Fix x y (Interp c) = Interp (Fix (Store Text Val,(StdGen,x)) (Error String (Store Text Val,(StdGen,y))) c)

runInterp :: Interp c x y -> c (Store Text Val, (StdGen,x)) (Error String (Store Text Val, (StdGen,y)))
runInterp (Interp f) = runExcept (runStore (runState f))

run :: [Statement] -> Error String (Store Text Val)
run ss = fst <$> runFixPoint (runInterp (Shared.run :: Fix [Statement] () (Interp (->)) [Statement] ())) (S.empty,(mkStdGen 0,ss))

instance ArrowChoice c => IsVal Val (Interp c) where
  boolLit = arr (\(b,_) -> BoolVal b)
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> failA -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> failA -< "Expected two booleans as arguments for 'or'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> failA -< "Expected a boolean as argument for 'not'"
  numLit = arr (\(d,_) -> NumVal d)
  randomNum = proc _ -> do
    gen <- getA -< ()
    let (r, gen') = random gen
    putA -< gen'
    returnA -< NumVal r
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> failA -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> failA -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 `Prelude.div` n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.== n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 P.== b2)
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.< n2)
    _ -> failA -< "Expected two numbers as arguments for 'lt'"

instance ArrowChoice c => Conditional Val x y z (Interp c) where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> failA -< "Expected boolean as argument for 'if'"

deriving instance ArrowChoice c => Category (Interp c)
deriving instance ArrowChoice c => Arrow (Interp c)
deriving instance ArrowChoice c => ArrowChoice (Interp c)
deriving instance ArrowChoice c => ArrowFail String (Interp c)
deriving instance ArrowChoice c => ArrowState StdGen (Interp c)
deriving instance (ArrowFix (Store Text Val,(StdGen,x)) (Error String (Store Text Val,(StdGen,y))) c, ArrowChoice c) => ArrowFix x y (Interp c)
deriving instance ArrowChoice c => ArrowStore Text Val Label (Interp c)

instance Hashable Val
