{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Structure.List
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module define a type for lists, in terms of 'Fix' from
-- "Fusion.Fix", along with some examples.
--
-----------------------------------------------------------------------------
module Tungsten.Structure.List where

import Prelude hiding (foldr, fromList, map, filter)
import qualified Prelude as Prelude

import Tungsten.Fix
import GHC.Base (build)

-- | The "factored-out" recursive type for lists
data ListF a b =
    NilF
  | ConsF a b
  deriving (Eq, Show, Functor)

-- | Lists
type List a = Fix (ListF a)

instance Show a => Show (List a) where
  show = cata go
    where
      go NilF = "nil"
      go (ConsF a b) = "cons ("++show a ++") ("++b++")"

-- | The Cons operator. Similar to 'Prelude.(:)' for Prelude lists.
cons :: a -> List a -> List a
cons x xs = fix (ConsF x xs)

-- | The empty list.  Similar to 'Prelud.[]' for Prelude lists.
nil :: List a
nil = fix NilF

-- | Transform a fixed-point list into a Prelude one.
-- | Can be fused with both good consumers of Prelude lists and good producers of fixed-point lists.
toList :: List a -> [a]
toList xs =
  build
  (\c n ->
     let go xs =
           case xs of
             NilF -> n
             ConsF a b -> c a b
     in cata go xs)

-- | Transform a Prelude list into a fixed-point one.
fromList :: [a] -> List a
fromList = Prelude.foldr cons nil

-- | Give the list of suffixes of a list.
suff :: List a -> List (List a)
suff = para go
  where
    go NilF = nil
    go (ConsF x (xs,b)) = cons (cons x xs) b

-- | The classical right fold. Can be fused with good producers of fixed-point lists.
foldr :: (a -> b -> b) -> b -> List a -> b
foldr c n = cata go
  where
    go NilF = n
    go (ConsF a b) = c a b
{-# INLINE foldr #-}

-- | The classical map. Can be fused with good producers and good consummers of fixed-point lists.
map :: (a -> b) -> List a -> List b
map f xs = buildR $ \c ->
  let go x =
        case x of
          NilF -> c NilF
          ConsF a b -> c (ConsF (f a) b)
  in cata go xs
{-# INLINE map #-}

-- | Can be fused with good producers of fixed-point lists.
-- | Can be fused with good producers and good consummers of fixed-point lists.
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
