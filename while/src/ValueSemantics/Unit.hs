{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ValueSemantics.Unit where

import           Prelude hiding (Bool(..),Bounded(..))

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared
import           ValueSemantics.Abstract

import           Data.Abstract.PropagateError (Error(..))
import           Data.Abstract.PreciseStore (Store)
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.Environment as E
import           Data.Abstract.Terminating

import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fix
import           Control.Arrow.Conditional
import           Control.Arrow.Random
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

-- Value semantics for the while language that does not approximate values at all.
type Addr = Label
type Val = ()

run :: [(Text,Addr)] -> [LStatement] -> Terminating (Error String (Store Addr Val))
run env ss =
  fmap fst <$>
    runLeastFix
      (runInterp
        (Shared.run :: Fix [Statement] () (Interp Addr Val (LeastFix () () (->))) [Statement] ()))
      (S.empty,(E.fromList env,generate <$> ss))

instance ArrowChoice c => ArrowAlloc (Text,Val,Label) Addr (Interp addr val c) where
  alloc = arr $ \(_,_,l) -> l

instance ArrowChoice c => IsVal Val (Interp addr val c) where
  boolLit = arr (const ())
  and = arr (const ())
  or = arr (const ())
  not = arr (const ())
  numLit = arr (const ())
  add = arr (const ())
  sub = arr (const ())
  mul = arr (const ())
  div = arr (const ())
  eq = arr (const ())
  lt = arr (const ())

instance ArrowChoice c => ArrowRand Val (Interp addr val c) where
  random = arr (const ())

instance (Complete (Interp addr val c (x,y) z), ArrowChoice c)
  => ArrowCond Val x y z (Interp addr val c) where
  if_ f1 f2 = proc (_,(x,y)) -> joined f1 f2 -< (x,y)
