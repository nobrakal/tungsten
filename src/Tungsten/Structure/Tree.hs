{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Structure.Tree
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module define a type for binary trees, in terms of 'Fix' from
-- "Tungsten.Fix", along with some examples.
--
-----------------------------------------------------------------------------
module Tungsten.Structure.Tree where

import Tungsten.Fix

-- | The "factored-out" recursive type for binary trees
data TreeF a b =
    EmptyF
  | LeafF a
  | NodeF b b
  deriving (Eq, Show, Functor)

-- | Binary trees
type Tree a = Fix (TreeF a)

empty :: Tree a
empty = fix EmptyF
{-# INLINE empty #-}

leaf :: a -> Tree a
leaf = fix . LeafF
{-# INLINE leaf #-}

node :: Tree a -> Tree a -> Tree a
node = \a b -> fix (NodeF a b)
{-# INLINE node #-}

instance Show a => Show (Tree a) where
  show = cata go
    where
      go EmptyF = "empty"
      go (LeafF a) = "leaf ( " ++ show a ++ ")"
      go (NodeF a b) = "node (" ++ a ++ ") (" ++ b ++ ")"

-- | Fmap for Tree
mapt :: (a -> b) -> Tree a -> Tree b
mapt f t = bind t (fix . LeafF . f)
{-# INLINE mapt #-}

-- | Bind for Tree, defined in term of 'buildR'
bind :: Tree a -> (a -> Tree b) -> Tree b
bind t f =
  buildR
  (\u ->
     cata
     (\x ->
         case x of
           EmptyF -> u EmptyF
           LeafF x -> cata u (f x)
           NodeF a b -> u $ NodeF a b)
     t)
{-# INLINE bind #-}

-- | @hasLeaf s t@ tests if the leaf @s@ is present in the tree @t@
-- | Can be fused with good producers of trees.
hasLeaf :: Eq a => a -> Tree a -> Bool
hasLeaf s = cata go
  where
    go EmptyF = False
    go (LeafF x) = x == s
    go (NodeF a b) = a || b
{-# INLINE hasLeaf #-}

-- | Construct a binary tree from a list.
-- | This function is subject to fusion with both good producers of (Prelude) lists and good consummers of trees.
treeFromList :: [Tree a] -> Tree a
treeFromList xs = buildR (\g -> foldr (\x -> g . NodeF (cata g x)) (g EmptyF) xs)
{-# INLINE treeFromList #-}

-- | 'leftTree n' construct a tree with n leaves from 1 to n.
-- | Can be fused with good consummers of trees.
leftTreeN :: Int -> Tree Int
leftTreeN = ana go . Right
  where
    go (Left n) = LeafF n
    go (Right n) =
      if n <= 0
      then EmptyF
      else NodeF (Left n) (Right $ n-1)
{-# INLINE leftTreeN #-}
