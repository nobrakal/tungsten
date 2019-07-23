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

import Data.Functor.Classes

import Tungsten.Fix

-- | The "factored-out" recursive type for binary trees.
data TreeF a b =
    EmptyF
  | LeafF a
  | NodeF b b
  deriving (Eq, Ord, Show, Read, Functor)

instance Eq2 TreeF where
  liftEq2 _ _ EmptyF      EmptyF        = True
  liftEq2 f _ (LeafF a)   (LeafF a')    = f a a'
  liftEq2 _ g (NodeF a b) (NodeF a' b') = g a a' && g b b'
  liftEq2 _ _ _           _             = False

instance Eq a => Eq1 (TreeF a) where
  liftEq = liftEq2 (==)

instance Show2 TreeF where
  liftShowsPrec2 sa _ sb _ d x =
    case x of
      EmptyF -> showString "EmptyF"
      LeafF a -> showParen (d > 10)
        $ showString "LeafF "
        . sa 11 a
      NodeF a b -> showParen (d > 10)
        $ showString "NodeF "
        . sb 11 a
        . showString " "
        . sb 11 b

instance Show a => Show1 (TreeF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Read2 TreeF where
  liftReadsPrec2 ra _ rb _ d = readParen (d > 10) $ \s -> nilf s ++ leaff s ++ nodef s
    where
      nilf s0 = do
        ("EmptyF", s1) <- lex s0
        return (EmptyF, s1)
      leaff s0 = do
        ("LeafF", s1) <- lex s0
        (a,       s2) <- ra 11 s1
        return (LeafF a, s2)
      nodef s0 = do
        ("NodeF", s1) <- lex s0
        (a,       s2) <- rb 11 s1
        (b,       s3) <- rb 11 s2
        return (NodeF a b, s3)

instance Read a => Read1 (TreeF a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

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

-- | 'fmap' for trees.
-- Good consumer and good producer.
mapt :: (a -> b) -> Tree a -> Tree b
mapt f t = bind t (fix . LeafF . f)
{-# INLINE mapt #-}

-- | @bind@ for trees.
-- Good consumer and good producer.
bind :: Tree a -> (a -> Tree b) -> Tree b
bind t f = buildR $ \fix' ->
  cata
  (\x ->
      case x of
        EmptyF -> fix' EmptyF
        LeafF a -> cata fix' $ f a
        NodeF a b -> fix' $ NodeF a b)
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
treeFromList xs = buildR $ \fix' ->
  foldr (\x -> fix' . NodeF (cata fix' x)) (fix' EmptyF) xs
{-# INLINE treeFromList #-}

-- | @leftTree n@ construct a tree with n leaves from 1 to n.
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
