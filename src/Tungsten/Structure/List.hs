{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, DeriveFunctor, OverloadedLists #-}
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
  ( -- * Lists as fixed-points
    ListF (..), List (..)
  , nil, cons

  -- * Classical operations on lists
  , foldr, map, append

  -- * Operations on lists
  , elem, range

  -- * Conversions
  , toList, fromList
  )
where

import Data.Functor.Classes

import Prelude hiding (foldr, map, elem, sum)
import qualified Prelude as Prelude

import Data.Coerce (coerce)

import Tungsten.Fix
import GHC.Base (build)
import qualified GHC.Exts as Ext

-- | The factored-out recursive type for lists.
data ListF a b =
    NilF
  | ConsF a b
  deriving (Eq, Ord, Show, Read, Functor)

instance Eq2 ListF where
  liftEq2 _ _ NilF        NilF          = True
  liftEq2 f g (ConsF a b) (ConsF a' b') = f a a' && g b b'
  liftEq2 _ _ _          _            = False

instance Eq a => Eq1 (ListF a) where
  liftEq = liftEq2 (==)

instance Ord2 ListF where
  liftCompare2 _ _ NilF        NilF          = EQ
  liftCompare2 _ _ NilF        _             = LT
  liftCompare2 _ _ _           NilF          = GT
  liftCompare2 f g (ConsF a b) (ConsF a' b') = f a a' `mappend` g b b'

instance Ord a => Ord1 (ListF a) where
  liftCompare = liftCompare2 compare

instance Show2 ListF where
  liftShowsPrec2 sa _ sb _ d x =
    case x of
      NilF -> showString "NilF"
      (ConsF a b) -> showParen (d > 10)
        $ showString "ConsF "
        . sa 11 a
        . showString " "
        . sb 11 b

instance Show a => Show1 (ListF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | Linked lists as a fixed-point.
newtype List a = List (Fix (ListF a))

instance Eq a => Eq (List a) where
  (List xs) == (List ys) = xs == ys

instance Ord a => Ord (List a) where
  compare (List xs) (List ys) = compare xs ys

instance Show a => Show (List a) where
  show (List xs) = show xs

instance Functor List where
  fmap = map

instance Applicative List where
  pure x = cons x nil
  fs <*> xs = foldr (\f acc -> foldr (\x -> cons (f x)) acc xs) nil fs

-- | `>>=` is a good consumer and producer.
instance Monad List where
  return = pure
  (>>=) = bind

instance Ext.IsList (List a) where
  type (Item (List a)) = a
  fromList = fromList
  toList = toList

-- | The empty list. Similar to 'Prelude.[]' for Prelude lists.
nil :: List a
nil = List (fix NilF)

-- | The cons operator. Similar to 'Prelude.(:)' for Prelude lists.
cons :: a -> List a -> List a
cons x (List xs) = List (fix (ConsF x xs))

-- | The classical right fold. Good consumer.
foldr :: (a -> b -> b) -> b -> List a -> b
foldr c n = cata go . coerce
  where
    go NilF = n
    go (ConsF a b) = c a b
{-# INLINE foldr #-}

-- | The classical map.
-- Good consumer and good producer.
map :: (a -> b) -> List a -> List b
map f xs = coerce $ buildR $ \fix' ->
  let go x =
        case x of
          NilF -> fix' NilF
          ConsF a b -> fix' $ ConsF (f a) b
  in cata go (coerce xs)
{-# INLINE map #-}

-- | Append two lists.
-- Good consumers of both arguments and producer.
append :: List a -> List a -> List a
append (List xs) ys = coerce $ buildR $ \fix' ->
  let go x =
        case x of
          NilF -> cata fix' (coerce ys)
          ConsF a b -> fix' $ ConsF a b
  in cata go xs
{-# INLINE append #-}

-- bind
bind :: List a -> (a -> List b) -> List b
bind (List xs) f = List $ buildR $ \fix' ->
  let append' xs' ys' =
        let go x =
              case x of
                NilF -> ys'
                ConsF a b -> fix' (ConsF a b)
        in cata go xs'
      go' x =
        case x of
          NilF -> fix' NilF
          ConsF a b -> append' (coerce (f a)) b
  in cata go' xs
{-# INLINE bind #-}

-- | Search an element in a list.
-- Good consumer.
elem :: Eq a => a -> List a -> Bool
elem e = cata go . coerce
  where
    go NilF = False
    go (ConsF a b) = a == e || b
{-# INLINE elem #-}

-- | @range start end@ will produce a list containing int
-- in ascending order from @start@ (inclusive) to @end@ (exclusive).
-- Good producer.
range :: Int -> Int -> List Int
range start end = coerce $ ana go start
  where
    go n =
      if n > end
      then NilF
      else ConsF n (n+1)
{-# INLINE range #-}

-- | Transform a fixed-point list into a Prelude one.
-- Good producer (of Prelude lists) and good consumer (of fixed-point lists).
toList :: List a -> [a]
toList xs =
  build
  (\c n ->
     let go ys =
           case ys of
             NilF -> n
             ConsF a b -> c a b
     in cata go (coerce xs))
{-# INLINE toList #-}

-- | Transform a Prelude list into a fixed-point one.
-- Good producer (fixed-point lists) and good consumer of (of Prelude lists).
fromList :: [a] -> List a
fromList xs = coerce $ buildR $ \fix' -> Prelude.foldr (\x -> fix' . ConsF x) (fix' NilF) xs
{-# INLINE fromList #-}
