{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Tree where

import Tungsten.Fix
import Tungsten.Structure.Tree

import Test.Inspection

foldBind, foldBindR :: (TreeF b p -> p) -> (a -> SoftTree b) -> Tree a -> p
foldBind g f t = cata g (bind (soften t) f)
foldBindR g f t = fold go t
  where
    go EmptyF = g EmptyF
    go (LeafF x) = (f x) g
    go (NodeF a b) = g (NodeF a b)

inspect $ 'foldBind === 'foldBindR

foldFmap, foldFmapR :: (TreeF b p -> p) -> (a -> b) -> Tree a -> p
foldFmap g f t = cata g (mapt f (soften t))
foldFmapR g f t = fold go t
  where
    go EmptyF = g EmptyF
    go (LeafF x) = g (LeafF (f x))
    go (NodeF a b) = g $ NodeF a b

inspect $ 'foldFmap === 'foldFmapR
