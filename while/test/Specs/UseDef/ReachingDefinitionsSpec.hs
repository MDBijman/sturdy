module Specs.UseDef.ReachingDefinitionsSpec(main,spec) where

import WhileLanguageSoundness

import qualified Props.UseDef.Concrete as ConcreteTrace
import qualified Props.UseDef.ReachingDefinitions.Concrete as ConcreteAnalyze
import qualified Props.UseDef.ReachingDefinitions.Interval as IntervalAnalyze
import Props.UseDef.ReachingDefinitions.Prop ()

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  whileSoundnessSpec ConcreteTrace.runLifted ConcreteAnalyze.runLifted
--  whileSoundnessSpec ConcreteAnalyze.runLifted IntervalAnalyze.run
  whileSoundnessSpec ConcreteTrace.runLifted IntervalAnalyze.run

soundness check failed:
Success [Label {labelVal = -2} -> ReachingDefs {defs = fromList [("",fromList [Label {labelVal = 1}])]}, Label {labelVal = 1} -> ReachingDefs {defs = fromList []}, Label {labelVal = 2} -> ReachingDefs {defs = fromList [("",fromList [Label {labelVal = 1}])]}]
 ⊑
Success [Label {labelVal = 1} -> ReachingDefs {defs = fromList [("",fromList [Label {labelVal = 1}])]}, Label {labelVal = 2} -> ReachingDefs {defs = fromList [("",fromList [Label {labelVal = 2}])]}] (after 1 test):
