{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Structure.Graph
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module defines a type isomorphic to algebraic graphs (from A. Mokhov, see the
-- <http://hackage.haskell.org/package/algebraic-graphs algebraic-graphs package>),
-- in terms of 'Fix' from "Tungsten.Fix".
--
-- A good consumer is a function that can be fused with a good producer.
-- A good producer is a function that can be fused with a good consumer.
--
-----------------------------------------------------------------------------
module Tungsten.Structure.Graph
  ( -- * Algebraic graphs as fixed-point
    GraphF (..), Graph
  , empty, vertex, overlay, connect

  -- * Classical operations on graphs
  , foldg, mapg, bind

  -- * Operations on graphs
  , hasVertex, edges
  )
where

import Data.Functor.Classes

import Tungsten.Fix

-- | The "factored-out" recursive type for algebraic graphs.
data GraphF a b =
    EmptyF
  | VertexF a
  | OverlayF b b
  | ConnectF b b
  deriving (Eq, Ord, Show, Read, Functor)

instance Eq2 GraphF where
  liftEq2 _ _ EmptyF         EmptyF           = True
  liftEq2 f _ (VertexF a)    (VertexF a')     = f a a'
  liftEq2 _ g (OverlayF a b) (OverlayF a' b') = g a a' && g b b'
  liftEq2 _ g (ConnectF a b) (ConnectF a' b') = g a a' && g b b'
  liftEq2 _ _ _           _                   = False

instance Eq a => Eq1 (GraphF a) where
  liftEq = liftEq2 (==)

instance Show2 GraphF where
  liftShowsPrec2 sa _ sb _ d x =
    case x of
      EmptyF -> showString "EmptyF"
      VertexF a -> showParen (d > 10)
        $ showString "VertexF "
        . sa 11 a
      OverlayF a b -> showParen (d > 10)
        $ showString "OverlayF "
        . sb 11 a
        . showString " "
        . sb 11 b
      ConnectF a b -> showParen (d > 10)
        $ showString "ConnectF "
        . sb 11 a
        . showString " "
        . sb 11 b

instance Show a => Show1 (GraphF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

type Graph a = Fix (GraphF a)

-- | The empty graph.
empty :: Graph a
empty = fix EmptyF
{-# INLINE empty #-}

-- | A vertex.
vertex :: a -> Graph a
vertex = fix . VertexF
{-# INLINE vertex #-}

-- | Overlay two graphs.
overlay :: Graph a -> Graph a -> Graph a
overlay = \a b -> fix (OverlayF a b)
{-# INLINE overlay #-}

-- | Connect two graphs.
connect :: Graph a -> Graph a -> Graph a
connect = \a b -> fix (ConnectF a b)
{-# INLINE connect #-}

-- | Fold a graph. Good consumer.
foldg :: b -- ^ Empty case
      -> (a -> b) -- ^ Vertex case
      -> (b -> b -> b) -- ^ Overlay case
      -> (b -> b -> b) -- ^ Connect case
      -> Graph a -- ^ The graph to fold
      -> b
foldg e v o c = cata go
  where
    go EmptyF = e
    go (VertexF x) = v x
    go (OverlayF a b) = o a b
    go (ConnectF a b) = c a b
{-# INLINE foldg #-}

-- | 'fmap' for algebraic graphs.
-- Good consumer and good producer.
mapg :: (a -> b) -> Graph a -> Graph b
mapg f g = bind g (vertex . f)
{-# INLINE mapg #-}

-- | @bind@ for algebraic graphs.
-- Good consumer and good producer.
bind :: Graph a -> (a -> Graph b) -> Graph b
bind t f = buildR $ \fix' ->
  cata
  (\x ->
      case x of
        EmptyF -> fix' EmptyF
        VertexF a -> cata fix' $ f a
        OverlayF a b -> fix' $ OverlayF a b
        ConnectF a b -> fix' $ ConnectF a b)
  t
{-# INLINE bind #-}

-- | Test if a vertex is in a graph.
-- Good consumer.
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex v = cata go
  where
    go EmptyF = False
    go (VertexF x) = v == x
    go (OverlayF a b) = a || b
    go (ConnectF a b) = a || b
{-# INLINE hasVertex #-}

-- | Construct a graph from a list of edges
-- Good consumer of lists and producer of graphs.
edges :: [(a,a)] -> Graph a
edges xs = buildR $ \fix' ->
  let edge' (u,v) = (fix' $ ConnectF (fix' (VertexF u)) (fix' (VertexF v)))
  in foldr (\e -> fix' . OverlayF (edge' e)) (fix' EmptyF) xs
{-# INLINE edges #-}
