{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix(ArrowFix(..),Fix,ArrowFix'(..)) where

import Control.Arrow

-- | Arrow-based interface for describing fixpoint computations.
class Arrow c => ArrowFix x y c where
  -- | Computes the fixpoint of an arrow computation.
  fixA :: (c x y -> c x y) -> c x y

instance ArrowFix x y (->) where
  fixA f = f (fixA f)

-- | Arrow-based interface for describing fixpoint computations used by Stratego.
class Arrow c => ArrowFix' c y | c -> y where
  fixA' :: ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)

-- | Computes the type of the fixpoint cache used by 'LeastFixPoint'.
--
-- For the concrete interpreter use 'Fix' with '->' as last component of the arrow transformer stack:
-- @
--   Fix Expr Val (State Store (->)) x y = State Store (->) x y
-- @
--
-- For the abstract interpreter use 'Fix' with '~>' as last component of the arrow transformer stack:
-- @
--   Fix Expr Val (State Store (~>)) x y = State Store (LeastFixPoint (Store,Expr) (Store,Val))
-- @
type family Fix x y (c :: * -> * -> *) :: * -> * -> *
