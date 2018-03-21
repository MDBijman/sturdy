{-# LANGUAGE DeriveGeneric #-}
module Props.UseDef.Prop where

import WhileLanguage

import Data.Text (Text)
import Data.Powerset

import Data.Hashable
import GHC.Generics (Generic)

---------------
-- Trace: Use and def events
---------------

data TrUseDef v =
  -- A use of variable `text` with `label`
  TrUse Text Label |
  -- A definition of variable `text` at assignment `label`
  TrDef Text v Label
  deriving (Eq,Show,Generic)

useDefName :: TrUseDef -> Text
useDefName (TrUse t _) = t
useDefName (TrDef t _) = t

useDefLabel :: TrUseDef -> Label
useDefLabel (TrUse _ l) = l
useDefLabel (TrDef _ l) = l

instance Hashable TrUseDef where

type Trace v = [TrUseDef v]

initTrace :: Trace
initTrace = []

type LiftedTrace = Pow Trace

liftTrace :: Trace -> LiftedTrace
liftTrace = singleton

-- ---------------
-- -- Prop: use-def/def-use relations
-- ---------------
--
-- data UseDef = UseDef { useDef :: Map Label (Set Label), defUse :: Map Label (Set Label) }
--   deriving (Eq,Show,Generic)
--
-- instance Hashable UseDef where
--
-- extractStatementUseDef :: Trace -> UseDef
-- extractStatementUseDef = snd . foldl go (Map.empty, UseDef Map.empty Map.empty)
--   where go :: (Map Text Label, UseDef) -> TrUseDef -> (Map Text Label, UseDef)
--         go (env, ud) (TrDef v def) = (Map.insert v def env, ud)
--         go (env, UseDef ud du) (TrUse v use) =
--           (env, UseDef (Map.insertWith Set.union use (Set.singleton def) ud)
--                        (Map.insertWith Set.union def (Set.singleton use) du))
--           where def = env Map.! v
