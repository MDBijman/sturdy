{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Props.UseDef.ReachingDefinitions.Prop where

import Label
import Data.Order
import Data.GaloisConnection
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Powerset
import qualified Data.Store as S

import Props.UseDef.Prop


newtype ReachingDefs = ReachingDefs {
  -- For each variable, set of assignments that can reach the current execution point
  defs :: Map Text (Set Label)
} deriving (Eq,Show)

instance PreOrd ReachingDefs where
  rd1 ⊑ rd2 = defs rd1 ⊑ defs rd2
  (≈) = (==)

instance Complete ReachingDefs where
  rd1 ⊔ rd2 = ReachingDefs $ defs rd1 ⊔ defs rd2

instance LowerBounded ReachingDefs where
  bottom = ReachingDefs Map.empty

type Prop = ReachingDefs

---------------
-- Galois
---------------

-- relate use-def traces to ReachingDefs after very last statement
instance Galois (Pow Trace) ReachingDefs where
  alpha = lifted lift
    where lift trace = ReachingDefs $ foldl extendDefs bottom trace
          extendDefs ds (TrUse _ _) = ds
          extendDefs ds (TrDef x l) = Map.insert x (Set.singleton l) ds

  gamma = undefined

-- relate use-def traces to ReachDefs after each statement
instance Galois (Pow Trace) (S.Store Label ReachingDefs) where
  alpha = lifted lift
    where lift trace = extendStore bottom (length trace - 1) trace
          extendStore st k trace | k < 0 = st
                                 | otherwise = extendStore st' (k-1) trace
            where lab = useDefLabel $ trace !! k
                  ds = alpha (singleton $ take k trace)
                  st' = S.insertWith (⊔) lab ds st

  gamma = undefined

-- alternative definition, which does not use last-statement Galois
--instance Galois (Pow Trace) (S.Store Label ReachingDefs) where
--  alpha = lifted (lift bottom bottom)
--    where lift :: ReachingDefs -> S.Store Label ReachingDefs -> Trace -> S.Store Label ReachingDefs
--          lift _ st [] = st
--          lift ds st (TrUse _ l : ts) = lift ds (S.insertWith (⊔) l ds st) ts
--          lift ds st (TrDef x l : ts) = lift ds' (S.insertWith (⊔) l ds' st) ts
--            where ds' = ReachingDefs $ Map.insert x (Set.singleton l) (defs ds)
--
--  gamma = undefined
