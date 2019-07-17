{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Structure.List
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module defines a type isomorphic to linked lists, in terms of 'Fix' from
-- "Tungsten.Fix".
--
-- A good consumer is a function that can be fused with a good producer.
-- A good producer is a function that can be fused with a good consumer.
--
-----------------------------------------------------------------------------
module Tungsten.Structure.List
  ( -- * Lists as fixed-point
    ListF (..), List
  , nil, cons

  -- * Classical operations
  , foldr, map, elem, range

  -- * Conversion
  , toList, fromList
  )
where

import Prelude hiding (foldr, map, elem, sum)
import qualified Prelude as Prelude

import Tungsten.Fix
import GHC.Base (build)

-- | The factored-out recursive type for lists.
data ListF a b =
    NilF
  | ConsF a b
  deriving (Eq, Show, Functor)

-- | Linked lists as a fixed-point.
type List a = Fix (ListF a)

-- | 'show' is a good consumer.
instance Show a => Show (List a) where
  show = cata go
    where
      go NilF = "nil"
      go (ConsF a b) = "cons ("++show a ++") ("++b++")"

-- | The empty list. Similar to 'Prelude.[]' for Prelude lists.
nil :: List a
nil = fix NilF

-- | The cons operator. Similar to 'Prelude.(:)' for Prelude lists.
cons :: a -> List a -> List a
cons x xs = fix (ConsF x xs)

-- | The classical right fold. Good consumer.
foldr :: (a -> b -> b) -> b -> List a -> b
foldr c n = cata go
  where
    go NilF = n
    go (ConsF a b) = c a b
{-# INLINE foldr #-}

-- | The classical map.
-- Good consumer and good producer.
map :: (a -> b) -> List a -> List b
map f xs = buildR $ \c ->
  let go x =
        case x of
          NilF -> c NilF
          ConsF a b -> c (ConsF (f a) b)
  in cata go xs
{-# INLINE map #-}

-- | Search an element in a list.
-- Good consumer.
elem :: Eq a => a -> List a -> Bool
elem e = cata go
  where
    go NilF = False
    go (ConsF a b) = a == e || b
{-# INLINE elem #-}

-- | @range start end@ will produce a list containing int
-- in ascending order from @start@ (inclusive) to @end@ (exclusive).
-- Good producer.
range :: Int -> Int -> List Int
range start end = ana go start
  where
    go n =
      if n > end
      then NilF
      else ConsF n (n+1)
{-# INLINE range #-}

-- | Transform a fixed-point list into a Prelude one.
-- Good producer (of Prelude lists) and good consumer (of fixed-point lists).
fromList :: List a -> [a]
fromList xs =
  build
  (\c n ->
     let go ys =
           case ys of
             NilF -> n
             ConsF a b -> c a b
     in cata go xs)
{-# INLINE fromList #-}

-- | Transform a Prelude list into a fixed-point one.
-- Good producer (fixed-point lists) and good consumer of (of Prelude lists).
toList :: [a] -> List a
toList xs = buildR $ \c -> Prelude.foldr (\x ys -> c (ConsF x ys)) (c NilF) xs
{-# INLINE toList #-}
