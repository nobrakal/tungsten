{-# LANGUAGE DeriveFunctor #-}
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
  ( -- * Algebraic graphs as fixed-points
    GraphF (..), Graph (..)
  , empty, vertex, overlay, connect

  -- * Classical operations on graphs
  , foldg

  -- * Operations on graphs
  , transpose, hasVertex

  -- * Conversions
  , vertices, edges
  )
where

import Data.Functor.Classes

import Data.Coerce (coerce)

import Tungsten.Fix

-- | The "factored-out" recursive type for algebraic graphs.
data GraphF a b =
    EmptyF
  | VertexF a
  | OverlayF b b
  | ConnectF b b
  deriving (Eq, Ord, Show, Read, Functor)

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

newtype Graph a = Graph (Fix (GraphF a))

instance Show a => Show (Graph a) where
  show (Graph g) = show g

instance Functor Graph where
  fmap = mapg

instance Applicative Graph where
  pure = vertex
  (Graph f) <*> (Graph x) = Graph $ buildR $ \fix' ->
    let e = fix' EmptyF
        o = overlayF fix'
        c = overlayF fix'
    in
    cata (graphF e (\f' -> cata (graphF e (fix' . VertexF . f') o c) x) o c) f

instance Monad Graph where
  return = pure
  (>>=) = bind

-- | The empty graph.
empty :: Graph a
empty = Graph $ fix EmptyF
{-# INLINE empty #-}

-- | A vertex.
vertex :: a -> Graph a
vertex = Graph . fix . VertexF
{-# INLINE vertex #-}

-- | Overlay two graphs.
overlay :: Graph a -> Graph a -> Graph a
overlay (Graph a) (Graph b) = Graph $ fix (OverlayF a b)
{-# INLINE overlay #-}

-- | Connect two graphs.
connect :: Graph a -> Graph a -> Graph a
connect (Graph a) (Graph b) = Graph $ fix (ConnectF a b)
{-# INLINE connect #-}

graphF :: p -> (t1 -> p) -> (t2 -> t2 -> p) -> (t2 -> t2 -> p) -> GraphF t1 t2 -> p
graphF e _ _ _ EmptyF = e
graphF _ v _ _ (VertexF x) = v x
graphF _ _ o _ (OverlayF a b) = o a b
graphF _ _ _ c (ConnectF a b) = c a b
{-# INLINE graphF #-}

-- | Fold a graph. Good consumer.
foldg :: b -- ^ Empty case
      -> (a -> b) -- ^ Vertex case
      -> (b -> b -> b) -- ^ Overlay case
      -> (b -> b -> b) -- ^ Connect case
      -> Graph a -- ^ The graph to fold
      -> b
foldg e v o c = cata (graphF e v o c) . coerce
{-# INLINE foldg #-}

overlayF :: (GraphF a b -> t) -> b -> b -> t
overlayF f = \a b -> f (OverlayF a b)

connectF :: (GraphF a b -> t) -> b -> b -> t
connectF f = \a b -> f (ConnectF a b)

-- | 'fmap' for algebraic graphs.
-- Good consumer and good producer.
mapg :: (a -> b) -> Graph a -> Graph b
mapg f (Graph g) = Graph $ buildR $ \fix' ->
  let go = fix' . graphF EmptyF (VertexF . f) OverlayF ConnectF
  in cata go g
{-# INLINE mapg #-}

-- | @bind@ for algebraic graphs.
-- Good consumer and good producer.
bind :: Graph a -> (a -> Graph b) -> Graph b
bind (Graph g) f = Graph $ buildR $ \fix' ->
  let go = graphF (fix' EmptyF) (cata fix' . coerce . f) (overlayF fix') (connectF fix')
  in cata go g
{-# INLINE bind #-}

-- | Transpose a graph.
-- Good consumer and good producer.
transpose :: Graph a -> Graph a
transpose (Graph g) = Graph $ buildR $ \fix' ->
  let go x =
        case x of
          ConnectF a b -> fix' $ ConnectF b a
          _            -> fix' x
  in cata go g
{-# INLINE transpose #-}

-- | Test if a vertex is in a graph.
-- Good consumer.
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex v = cata (graphF False (==v) (||) (||)) . coerce
{-# INLINE hasVertex #-}

-- | Construct the graph comprising a given list of isolated vertices.
-- Good consumer of lists and producer of graphs.
vertices :: [a] -> Graph a
vertices xs = Graph $ buildR $ \fix' ->
  foldr (\x -> fix' . OverlayF (fix' (VertexF x))) (fix' EmptyF) xs
{-# INLINE vertices #-}

-- | Construct a graph from a list of edges
-- Good consumer of lists and producer of graphs.
edges :: [(a,a)] -> Graph a
edges xs = Graph $ buildR $ \fix' ->
  let edge' (u,v) = (fix' $ ConnectF (fix' (VertexF u)) (fix' (VertexF v)))
  in foldr (\e -> fix' . OverlayF (edge' e)) (fix' EmptyF) xs
{-# INLINE edges #-}
