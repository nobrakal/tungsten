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
-----------------------------------------------------------------------------
module Tungsten.Structure.List
  ( -- * Lists as fixed-point
    ListF (..), List
  , nil, cons

  -- * Classical operations
  , foldr, map, filter

  -- * Conversion
  , toList, fromList
  )
where

import Prelude hiding (foldr, fromList, map, filter)
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

-- | The classical right fold. Can be fused with good producers of lists.
foldr :: (a -> b -> b) -> b -> List a -> b
foldr c n = cata go
  where
    go NilF = n
    go (ConsF a b) = c a b
{-# INLINE foldr #-}

-- | The classical map.
-- Can be fused with good producers and good consummers of lists.
map :: (a -> b) -> List a -> List b
map f xs = buildR $ \c ->
  let go x =
        case x of
          NilF -> c NilF
          ConsF a b -> c (ConsF (f a) b)
  in cata go xs
{-# INLINE map #-}

-- | The filter operation
-- Can be fused with good producers and good consummers of lists.
filter :: (a -> Bool) -> List a -> List a
filter p xs = buildR $ \c ->
  let go x =
        case x of
          NilF -> c NilF
          ConsF a b ->
            if p a
            then c (ConsF a b)
            else b
  in cata go xs
{-# INLINE filter #-}

-- | Transform a fixed-point list into a Prelude one.
-- Can be fused with both good producers of fixed-point lists and good consumers of Prelude lists.
fromList :: List a -> [a]
fromList xs =
  build
  (\c n ->
     let go xs =
           case xs of
             NilF -> n
             ConsF a b -> c a b
     in cata go xs)
{-# INLINE fromList #-}

-- | Transform a Prelude list into a fixed-point one.
-- Can be fused with both good producers of Prelude lists and good consumers of fixed-point lists.
toList :: [a] -> List a
toList xs = buildR $ \c -> Prelude.foldr (\x xs -> c (ConsF x xs)) (c NilF) xs
{-# INLINE toList #-}
