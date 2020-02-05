{-# LANGUAGE DeriveFunctor #-}
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
  ( -- * Binary trees as fixed-points
    TreeF (..), Tree (..)
  , empty, leaf, node

  -- * Operations on trees
  , hasLeaf

  -- * Conversions
  , treeFromList, leftTreeN
  )
where

import Data.Functor.Classes

import Data.Coerce (coerce)

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

-- | Binary trees expressed as a fixed-point.
newtype Tree a = Tree (Fix (TreeF a))

instance Eq a => Eq (Tree a) where
  (Tree x) == (Tree y) = x == y

instance Show a => Show (Tree a) where
  show (Tree x) = show x

-- | 'fmap' is a good consumer and good producer.
instance Functor Tree where
  fmap = mapt

instance Applicative Tree where
  pure = leaf
  (Tree x) <*> (Tree y) = Tree $ buildR $ \fix' ->
    let v f =
          let go' y =
                case y of
                  EmptyF -> fix' EmptyF
                  LeafF a -> fix' (LeafF (f a))
                  NodeF a b -> fix' $ NodeF a b
          in cata go' y
        go x =
          case x of
            EmptyF -> fix' EmptyF
            LeafF a -> v a
            NodeF a b -> fix' $ NodeF a b
    in cata go x

-- | `>>=` is a good consumer and good producer.
instance Monad Tree where
  return = pure
  (>>=) = bindt

-- | The empty tree.
empty :: Tree a
empty = Tree $ fix EmptyF
{-# INLINE empty #-}

-- | A leaf.
leaf :: a -> Tree a
leaf = Tree . fix . LeafF
{-# INLINE leaf #-}

-- | A node.
node :: Tree a -> Tree a -> Tree a
node = \a b -> Tree $ fix $ NodeF (coerce a) (coerce b)
{-# INLINE node #-}

mapt :: (a -> b) -> Tree a -> Tree b
mapt f x = coerce $ buildR $ \fix' ->
  let go x =
        case x of
        EmptyF -> fix' EmptyF
        LeafF a -> fix' (LeafF (f a))
        NodeF a b -> fix' $ NodeF a b
  in cata go (coerce x)
{-# INLINE mapt #-}

bindt :: Tree a -> (a -> Tree b) -> Tree b
bindt t f = Tree $ buildR $ \fix' ->
  cata
  (\x ->
      case x of
        EmptyF -> fix' EmptyF
        LeafF a -> cata fix' $ coerce $ f a
        NodeF a b -> fix' $ NodeF a b)
  (coerce t)
{-# INLINE bindt #-}

-- | @hasLeaf s t@ tests if the leaf @s@ is present in the tree @t@.
-- Good consumer.
hasLeaf :: Eq a => a -> Tree a -> Bool
hasLeaf s = cata go . coerce
  where
    go EmptyF = False
    go (LeafF x) = x == s
    go (NodeF a b) = a || b
{-# INLINE hasLeaf #-}

-- | Construct a binary tree from a list.
-- Good consumer (of Prelude lists) and good producer (of trees).
treeFromList :: [Tree a] -> Tree a
treeFromList xs = Tree $ buildR $ \fix' ->
  foldr (\x -> fix' . NodeF (cata fix' (coerce x))) (fix' EmptyF) xs
{-# INLINE treeFromList #-}

-- | @leftTree n@ construct a tree with n leaves from 1 to n.
-- Good producer.
leftTreeN :: Int -> Tree Int
leftTreeN n = coerce $ ana go (Right 1)
  where
    go (Left n') = LeafF n'
    go (Right n') =
      if n' <= n
      then NodeF (Left n') (Right (n' + 1))
      else EmptyF
{-# INLINE leftTreeN #-}
