{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Tungsten.Fix
-- Copyright  : (c) Alexandre Moine 2019
-- Maintainer : alexandre@moine.me
-- Stability  : experimental
--
-- This module provides the 'Fix' operator which can be used to define
-- fixed-point structures
-- (see examples in "Tungsten.Structure.List" or "Tungsten.Structure.Tree").
--
-- Defining a type in term of 'Fix' gives access to 'cata' and 'buildR'
-- and the \"cata/buildR\" rewrite rule (see comment for 'buildR' for how to
-- use it).
--
-- To use efficiently this module, compile with rewrite rules enabled.
--
-----------------------------------------------------------------------------
module Tungsten.Fix
  ( -- * The fixed-point operator
    Fix (..)
  , fix, unfix

    -- * Recursion-schemes
  , cata, para, ana, apo, hylo

    -- * Tools for rewriting
  , Cata, buildR
  )
where

import Data.Coerce

-- | Operator to define fixed-point types.
newtype Fix f = Fix (f (Fix f))

-- | Remove one level of fixed-point.
unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f
{-# INLINE unfix #-}

-- | A synonym for 'Fix'.
fix :: f (Fix f) -> Fix f
fix = Fix
{-# INLINE fix #-}

-- | Catamorphism.
-- Functions defined in terms of 'cata' (or \"good consumers\") are subject to fusion with functions exprimed in terms of 'buildR' (or \"good producers\").
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = c
  where
    c = f . fmap c . unfix
{-# INLINE [0] cata #-}

-- | Paramorphism.
-- Functions defined in terms of 'para' are /not/ subject to fusion.
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para t = p
  where
    p = t . fmap ((,) <*> p) . unfix

-- | Anamorphism.
-- Defined in terms of 'buildR', so subject to fusion with 'cata'.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana f b = buildR (\comb -> let c = comb . fmap c . f in c b)
{-# INLINE ana #-}

-- | Apomorphism.
-- Functions defined in terms of 'apo' are /not/ subject to fusion.
apo :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo g = a where a = fix . (fmap (either id a)) . g

-- | Hylomorphism.
--
-- @
-- hylo f g == 'cata' f . 'ana' g
-- @
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g x = cata f (ana g x)

-- | Type of arguments of 'buildR'.
type Cata f = forall a. (f a -> a) -> a

-- | 'buildR' abstracts the build of a structure with respect to the fixed-point
-- combinator, such that we have the following rewrite rule (named \"cata/buildR\"):
--
-- @
-- cata f (buildR g) = g f
-- @
--
-- When firing, this remove the build of an intermediate structure.
-- A function expressed with 'buildR' is called a /good producer/.
--
-- Note 1. Without rewriting, 'buildR' is just:
--
-- @
-- buildR g = g Fix
-- @
--
-- Note 2. The validity of the \"cata/buildR\" rule is guaranteed by [free theorems
-- of Wadler](https://doi.org/10.1145/99370.99404). They are known to fail in presence of
-- 'seq', be careful.
--
-- Note 3. If @g = cata@ and a rewriting did not happen,
-- then the \"cata/id\" rule will replace the @cata Fix@ obtained with the inlining
-- of 'buildR' by 'id'.
buildR :: Cata f -> Fix f
buildR g = g Fix
{-# INLINE [1] buildR #-}

{-# RULES
"cata/buildR" [~1] forall (f :: t b -> b) (g :: Cata t).
  cata f (buildR g) = g f

-- We cannot target `Fix` since it will be optimized away
"cata/id"
  cata coerce = id
 #-}
