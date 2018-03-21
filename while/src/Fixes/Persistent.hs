{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fixes.Persistent where

import Prelude hiding (id, (.))

import WhileLanguage (Label, Statement, HasProp(..), label)

import Control.Arrow
import Control.Arrow.Fix
import Control.Category

import Data.Hashable
import Data.Order
import Data.Store (Store)
import qualified Data.Store as S
import Control.Arrow.Utils (injectBoth, eject)
import Control.Arrow.Fail
import Control.Arrow.State


newtype PersistentArrow k v c a b x y = PersistentArrow {
  runPersistent :: c (Store k v, x) (Store k v, y)
}

runPersistentInit :: Arrow c => PersistentArrow k v c a b x y -> c x (Store k v, y)
runPersistentInit f = proc x -> runPersistent f -< (S.empty,x)

instance Category c => Category (PersistentArrow k v c a b) where
  id = PersistentArrow id
  PersistentArrow f . PersistentArrow g = PersistentArrow (f . g)

liftPersistent :: Arrow c => c x y -> PersistentArrow k v c a b x y
liftPersistent f = PersistentArrow (second f)
{-# INLINE liftPersistent #-}

instance Arrow c => Arrow (PersistentArrow k v c a b) where
  arr f = liftPersistent (arr f)
  first (PersistentArrow f) = PersistentArrow $ (\(s,(x,y)) -> ((s,x),y)) ^>> first f >>^ (\((s',x'),y) -> (s',(x',y)))
  second (PersistentArrow f) = PersistentArrow $ (\(s,(x,y)) -> (x,(s,y))) ^>> second f >>^ (\(x,(s',y')) -> (s',(x,y')))

instance ArrowChoice c => ArrowChoice (PersistentArrow k v c a b) where
  left (PersistentArrow f) = PersistentArrow $ injectBoth ^>> left f >>^ eject
  right (PersistentArrow f) = PersistentArrow $ injectBoth ^>> right f >>^ eject

instance (ArrowChoice c, HasProp (PersistentArrow Label pr c [Statement] ()) pr, Complete pr) => ArrowFix [Statement] () (PersistentArrow Label pr c [Statement] ()) where
  fixA f = proc xs -> case xs of
      [] -> returnA -< ()
      x:xs2 -> do
        y <- f (fixA f) -< [x]
        let lab = label x
        pr <- getProp -< ()
        persist -< (lab, pr)
        returnA -< y
        fixA f -< xs2

persist :: (Arrow c, Eq x, Hashable x, Complete y) => PersistentArrow x y c a b (x,y) ()
persist = PersistentArrow $ arr $ \(st,(x,y)) -> (S.insertWith (⊔) x y st, ())

deriving instance PreOrd (c (Store k v,x) (Store k v,y)) => PreOrd (PersistentArrow k v c a b x y)
deriving instance LowerBounded (c (Store k v,x) (Store k v,y)) => LowerBounded (PersistentArrow k v c a b x y)
deriving instance Complete (c (Store k v,x) (Store k v,y)) => Complete (PersistentArrow k v c a b x y)
deriving instance CoComplete (c (Store k v,x) (Store k v,y)) => CoComplete (PersistentArrow k v c a b x y)
deriving instance UpperBounded (c (Store k v,x) (Store k v,y)) => UpperBounded (PersistentArrow k v c a b x y)


instance ArrowFail e c => ArrowFail e (PersistentArrow k v c a b) where
  failA = liftPersistent failA

instance ArrowState s c => ArrowState s (PersistentArrow k v c a b) where
  getA = liftPersistent getA
  putA = liftPersistent putA
