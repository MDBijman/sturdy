module Specs.IntervalSpec(main,spec) where

import qualified Vals.Concrete.Semantic as Concrete
import qualified Vals.Interval.Semantic as Interval

import WhileLanguageSoundness

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec Concrete.runLifted Interval.run
