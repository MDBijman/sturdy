{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Props.UseDef.ReachingDefinitions.Interval where

import Prelude (String, ($), (.), fst, snd, fmap)

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.UseDef.ReachingDefinitions.Prop

import Data.Error
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Order
import qualified Data.Store as S

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Utils

import Fixes.Persistent


store :: (ArrowChoice c, HasStore c Store, HasProp c Prop) => c (Text,Val,Label) ()
store = modifyProp (arr $ \((x,v,l),ReachingDefs ds) -> ReachingDefs $ Map.insert x (Set.singleton l) ds)
                         -- all previous defs of `x` are killed and `l` is generated
    &&> Interval.store


----------
-- Arrows
----------

type State = (Store,Prop)
initState :: State
initState = (initStore, bottom)

type In a = (State,a)
type Out a = Error String (State,a)
type M = PersistentArrow Label Prop (StateArrow State (ErrorArrow String (->))) [Statement] ()

runM :: [Statement] -> Error String (State, (S.Store Label Prop, ()))
runM ss = runErrorArrow (runStateArrow (runPersistentInit (L.run :: M [Statement] ()))) (initState, ss)

run :: [Statement] -> Error String (Store,S.Store Label Prop)
run = fmap (\((st,_), (pers,_)) -> (st,pers)) . runM

instance L.HasStore M Store where
  getStore = getA >>> arr fst
  putStore = modifyA $ arr $ \(st,(_,rnd)) -> (st,rnd)

instance L.HasProp M Prop where
  getProp = getA >>> arr snd
  putProp = modifyA $ arr $ \(pr,(st,_)) -> (st,pr)

instance L.Eval M Val  where
  lookup = Interval.lookup
  boolLit = Interval.boolLit
  and = Interval.and
  or = Interval.or
  not = Interval.not
  numLit = Interval.numLit
  randomNum = Interval.randomNum
  add = Interval.add
  sub = Interval.sub
  mul = Interval.mul
  div = Interval.div
  eq = Interval.eq
  fixEval = Interval.fixEval

instance L.Run M Val where
  store = store
  if_ = Interval.if_
