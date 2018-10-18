{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Data.Abstract.Powerset where

import           Prelude                hiding ((.))

import           Control.Applicative    hiding (empty)
import           Control.Category
import           Control.Monad

import           Data.Abstract.Widening
import           Data.Foldable          (foldl', toList)
import           Data.Hashable
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as H
import           Data.List              (intercalate)
import           Data.Order
<<<<<<< HEAD
<<<<<<< HEAD
import           Data.Sequence          (Seq, (<|))
=======
>>>>>>> upstream/widening
=======
>>>>>>> 09be9737a01730ed9c1882bf5d6795c570cf5dd1

import           GHC.Generics           (Generic)

newtype Pow a = Pow (Seq a) deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Monoid, Foldable, Traversable, Generic)

instance PreOrd a => PreOrd (Pow a) where
  as ⊑ bs = all (\x -> any (x ⊑) bs) as

instance (Eq a, Hashable a) => Eq (Pow a) where
  as == bs = toHashSet as == toHashSet bs

instance PreOrd a => Complete (Pow a) where
  as ⊔ bs = as `union` bs

instance PreOrd a => LowerBounded (Pow a) where
  bottom = empty

instance UpperBounded a => UpperBounded (Pow a) where
  top = singleton top

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> Data.Foldable.toList a) ++ "}"

instance (Eq a, Hashable a) => Hashable (Pow a) where
  hashWithSalt salt x = hashWithSalt salt (toHashSet x)

empty :: Pow a
empty = mempty

singleton :: a -> Pow a
singleton = Pow . return

insert :: a -> Pow a -> Pow a
insert a (Pow as) = Pow (a <| as)

union :: Pow a -> Pow a -> Pow a
union = mappend

cartesian :: (Pow a, Pow b) -> Pow (a,b)
cartesian (as,bs) = do
  a <- as
  b <- bs
  return (a,b)

toHashSet :: (Hashable a, Eq a) => Pow a -> HashSet a
toHashSet = foldl' (flip H.insert) H.empty

toList :: (Hashable a, Eq a) => Pow a -> [a]
toList = H.toList . toHashSet

fromFoldable :: (Foldable f, Monad t, Monoid (t a)) => f a -> t a
fromFoldable = foldMap return

size :: Foldable f => f a -> Int
size = length

dedup :: (Hashable a, Eq a) => Pow a -> Pow a
dedup = fromFoldable . toHashSet
