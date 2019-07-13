{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Fix
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module provide the type 'Fix' which can be used to define
-- fixed-point structures (see examples in "Tungsten.Structure.List" or "Tungsten.Structure.List").
--
-- Defining a type in term of 'Fix' gives access to 'cata' and 'buildR'
-- and the \"cata/buildR\" rewrite rule.
--
-----------------------------------------------------------------------------

module Tungsten.Fix where

import Data.Coerce

-- | Fixed point type
newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f
{-# INLINE unfix #-}

fix :: f (Fix f) -> Fix f
fix = Fix
{-# INLINE fix #-}

-- | Catamorphism. Functions defined in term of 'cata' are subject to fusion.
cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = c
  where
    c = f . fmap c . unfix
{-# INLINE [0] cata #-}

-- | Paramorphism
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para t = p
  where
    p = t . fmap ((,) <*> p) . unfix

-- | Anamorphism. Functions defined in term of 'ana' are subject to fusion.
ana :: Functor f => (b -> f b) -> b -> Fix f
ana f b = buildR (\comb -> let c = comb . fmap c . f in c b)
{-# INLINE ana #-}

-- | Hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g

-- Rewrite stuff
type Cata f = forall b. (f b -> b) -> b

-- | Defining a function in term of 'buildR' allows fusion if this function is composed
-- later with 'cata'.
buildR :: Cata f -> Fix f
buildR g = g fix
{-# INLINE [1] buildR #-}

{-# RULES
"cata/buildR" [~1] forall (f :: t b -> b) (g :: Cata t).
  cata f (buildR g) = g f

-- We cannot target `Fix` since it will be optimized away
"cata/id"
  cata coerce = id
 #-}
