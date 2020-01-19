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
-- TODO
--
-- To use efficiently this module, compile with rewrite rules enabled and
-- the @-fspec-constr@ flag.
--
-- A part of this module was inspired by the "Data.Functor.Foldable" module from the
-- <http://hackage.haskell.org/package/recursion-schemes-5.1.3 recursion-schemes package>.
--
-----------------------------------------------------------------------------
module Tungsten.Fix
  ( -- * The fixed-point operator
    Fix (..)
  , fix, unfix

    -- * Recursion-schemes
  , cata, para, ana, apo, hylo

    -- * Tools for rewriting
  , Soft, harden, soften
  )
where

import Data.Coerce
import Data.Functor.Classes
import Text.Read

import Data.Function (on)

-- | Operator to define fixed-point types.
newtype Fix f = Fix (f (Fix f))

instance Eq1 f => Eq (Fix f) where
  (==) = eq1 `on` unfix

instance Ord1 f => Ord (Fix f) where
  compare = compare1 `on` unfix

-- | 'show' is a good consumer.
instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec d x = cata go (soften x)
    where
      go a =
        showParen (d >= 11)
        $ showString "Fix "
        . liftShowsPrec
          (\d' -> showParen (d' >= 11))
          (foldr (.) id) 11 a

instance Read1 f => Read (Fix f) where
  readPrec = parens $ prec 10 $ do
    Ident "Fix" <- lexP
    Fix <$> step (readS_to_Prec readsPrec1)

-- | Remove one level of fixed-point.
unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f
{-# INLINE unfix #-}

-- | A synonym for 'Fix'.
fix :: f (Fix f) -> Fix f
fix = Fix
{-# INLINE fix #-}

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f = c
  where
    c = f . fmap c . unfix
{-# INLINE [0] fold #-}

-- | Type of arguments of 'cata'.
type Soft f = forall a. (f a -> a) -> a

-- | Catamorphism.
cata :: Functor f => (f a -> a) -> Soft f -> a
cata f x = x f
{-# INLINE cata #-}

-- | Paramorphism.
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para t = p
  where
    p = t . fmap ((,) <*> p) . unfix

-- | Anamorphism.
ana :: Functor f => (a -> f a) -> a -> Soft f
ana f b = \fix' -> let c = fix' . fmap c . f in c b
{-# INLINE ana #-}

-- | Apomorphism.
apo :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo g = a where a = fix . (fmap (either id a)) . g

-- | Hylomorphism.
--
-- @
-- hylo f g == 'cata' f . 'ana' g
-- @
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g x = cata f (ana g x)

-- | 'harden' a structure. Can be used at _no costs_ when postcomposing with
-- a function producing a 'Soft' result.
harden :: Soft f -> Fix f
harden g = g Fix
{-# INLINE harden #-}

-- | 'soften' a structure. Can be used at _no costs_ when precomposing with
-- a function taking a 'Soft' argument.
soften :: Functor f => Fix f -> Soft f
soften g = \f -> fold f g
{-# INLINE soften #-}

{-# RULES
-- We cannot target `Fix` since it will be optimized away
"cata/id"
  fold coerce = id
 #-}
