{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Structure.Tree
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module defines a type isomorphic to binary trees, in terms of 'Fix' from
-- "Tungsten.Fix".
--
-- A good consumer is a function that can be fused with a good producer.
-- A good producer is a function that can be fused with a good consumer.
--
-----------------------------------------------------------------------------
module Tungsten.Structure.Tree
  ( -- * Binary trees as fixed-point
    TreeF (..), Tree
  , empty, leaf, node

  -- * Classical operations on trees
  , mapt, bind

  -- * Operations on trees
  , hasLeaf, treeFromList, leftTreeN
  )
where

import Tungsten.Fix

-- | The "factored-out" recursive type for binary trees.
data TreeF a b =
    EmptyF
  | LeafF a
  | NodeF b b
  deriving (Eq, Show, Functor)

-- | Binary trees expressed as a fixed-point.
type Tree a = Fix (TreeF a)

-- | The empty tree.
empty :: Tree a
empty = fix EmptyF
{-# INLINE empty #-}

-- | A leaf.
leaf :: a -> Tree a
leaf = fix . LeafF
{-# INLINE leaf #-}

-- | A node.
node :: Tree a -> Tree a -> Tree a
node = \a b -> fix (NodeF a b)
{-# INLINE node #-}

instance Show a => Show (Tree a) where
  show = cata go
    where
      go EmptyF = "empty"
      go (LeafF a) = "leaf (" ++ show a ++ ")"
      go (NodeF a b) = "node (" ++ a ++ ") (" ++ b ++ ")"

-- | 'fmap' for trees.
-- Good consumer and good producer.
mapt :: (a -> b) -> Tree a -> Tree b
mapt f t = bind t (fix . LeafF . f)
{-# INLINE mapt #-}

-- | @bind@ for trees, defined in terms of 'buildR' and 'cata'.
-- Good consumer and good producer.
bind :: Tree a -> (a -> Tree b) -> Tree b
bind t f = buildR $ \u ->
  cata
  (\x ->
      case x of
        EmptyF -> u EmptyF
        LeafF a -> cata u (f a)
        NodeF a b -> u $ NodeF a b)
  t
{-# INLINE bind #-}

-- | @hasLeaf s t@ tests if the leaf @s@ is present in the tree @t@.
-- Good consumer.
hasLeaf :: Eq a => a -> Tree a -> Bool
hasLeaf s = cata go
  where
    go EmptyF = False
    go (LeafF x) = x == s
    go (NodeF a b) = a || b
{-# INLINE hasLeaf #-}

-- | Construct a binary tree from a list.
-- Good consumer (of Prelude lists) and good producer (of trees).
treeFromList :: [Tree a] -> Tree a
treeFromList xs = buildR $ \g -> foldr (\x -> g . NodeF (cata g x)) (g EmptyF) xs
{-# INLINE treeFromList #-}

-- | 'leftTree n' construct a tree with n leaves from 1 to n.
-- Good producer.
leftTreeN :: Int -> Tree Int
leftTreeN n = ana go (Right 1)
  where
    go (Left n') = LeafF n'
    go (Right n') =
      if n' <= n
      then NodeF (Left n') (Right (n' + 1))
      else EmptyF
{-# INLINE leftTreeN #-}
