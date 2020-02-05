{-# LANGUAGE TemplateHaskell #-}

module Tree where

import Tungsten.Fix
import Tungsten.Structure.Tree

import Test.Inspection

import Data.Coerce (coerce)

foldBind, foldBindR :: (TreeF b p -> p) -> (a -> Tree b) -> Tree a -> p
foldBind g f t = cata g (coerce (t >>= f))
foldBindR g f t = cata go (coerce t)
  where
    go EmptyF = g EmptyF
    go (LeafF x) = cata g $ coerce $ f x
    go (NodeF a b) = g $ NodeF a b

inspect $ 'foldBind === 'foldBindR

foldFmap, foldFmapR :: (TreeF b p -> p) -> (a -> b) -> Tree a -> p
foldFmap g f t = cata g (coerce (fmap f t))
foldFmapR g f t = cata go (coerce t)
  where
    go EmptyF = g EmptyF
    go (LeafF x) = g (LeafF (f x))
    go (NodeF a b) = g $ NodeF a b

inspect $ 'foldFmap === 'foldFmapR

dBind, dBindR :: (b -> Tree c) -> (a -> Tree b) -> Tree a -> Tree c
dBind g f t = (t >>= f) >>= g
dBindR g f t = t >>= (\x -> (f x) >>= g)

inspect $ 'dBind === 'dBindR
