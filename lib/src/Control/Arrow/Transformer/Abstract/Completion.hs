{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Completion(Completion(..)) where

import Prelude hiding ((.),id,lookup)

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Try
import Control.Category

import Data.Abstract.FreeCompletion
import Data.Monoidal
import Data.Order

-- | Allows to describe computations over non-completely ordered types.
-- E.g. allows to join a computation of type 'c x [y]'.
newtype Completion c x y = Completion { runCompletion :: c x (FreeCompletion y) }

instance ArrowLift Completion where
  lift f = Completion (f >>> arr Lower)

instance ArrowChoice c => Category (Completion c) where
  id = lift id
  Completion f . Completion g = Completion $ proc x -> do
    g' <- g -< x
    case g' of
      Lower a -> f -< a
      Top -> returnA -< Top

instance ArrowChoice c => Arrow (Completion c) where
  arr = lift . arr
  first (Completion f) = Completion $ first f >>^ strength1
  second (Completion f) = Completion $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (Completion c) where
  left (Completion f) = Completion $ left f >>^ strength1
  right (Completion f) = Completion $ right f >>^ strength2

instance (ArrowApply c, ArrowChoice c) => ArrowApply (Completion c) where
  app = Completion $ first runCompletion ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (Completion c) where
  getA = lift getA
  putA = lift putA

instance (ArrowChoice c, ArrowFail () c) => ArrowFail () (Completion c) where
  failA = lift failA

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (Completion c) where
  askA = lift askA
  localA (Completion f) = Completion (localA f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (Completion c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Completion f) = Completion (localEnv f)

instance ArrowChoice c => ArrowTry x y z (Completion c) where
  tryA (Completion f) (Completion g) (Completion h) = Completion $ proc x -> do
    e <- f -< x
    case e of
      Lower y -> g -< y
      Top -> h -< x

instance ArrowChoice c => ArrowDeduplicate (Completion c) where
  dedupA = returnA

deriving instance PreOrd (c x (FreeCompletion y)) => PreOrd (Completion c x y)
deriving instance LowerBounded (c x (FreeCompletion y)) => LowerBounded (Completion c x y)
deriving instance Complete (c x (FreeCompletion y)) => Complete (Completion c x y)
deriving instance CoComplete (c x (FreeCompletion y)) => CoComplete (Completion c x y)
deriving instance UpperBounded (c x (FreeCompletion y)) => UpperBounded (Completion c x y)
