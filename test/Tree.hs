{-# LANGUAGE TemplateHaskell #-}

module Tree where

import Tungsten.Fix
import Tungsten.Structure.Tree

import Test.Inspection

foldBind, foldBindR :: (TreeF b p -> p) -> (a -> Tree b) -> Tree a -> p
foldBind g f t = cata g (bind t f)
foldBindR g f t = cata go t
  where
    go EmptyF = g EmptyF
    go (LeafF x) = cata g $ f x
    go (NodeF a b) = g $ NodeF a b

inspect $ 'foldBind === 'foldBindR

foldFmap, foldFmapR :: (TreeF b p -> p) -> (a -> b) -> Tree a -> p
foldFmap g f t = cata g (mapt f t)
foldFmapR g f t = cata go t
  where
    go EmptyF = g EmptyF
    go (LeafF x) = g (LeafF (f x))
    go (NodeF a b) = g $ NodeF a b

inspect $ 'foldFmap === 'foldFmapR

dBind, dBindR :: (b -> Tree c) -> (a -> Tree b) -> Tree a -> Tree c
dBind g f t = bind (bind t f) g
dBindR g f t = bind t (\x -> bind (f x) g)

inspect $ 'dBind === 'dBindR

hasLeafleftTree,hasLeafleftTreeR :: Int -> Int -> Bool
hasLeafleftTree s n  = hasLeaf s (leftTreeN n)
hasLeafleftTreeR s n = go (Right 1)
  where
    go (Left n') = n' == s
    go (Right n') =
      if n' <= n
      then if (go (Left n')) then True else go (Right (n'+1))
      else False

inspect $ 'hasLeafleftTree === 'hasLeafleftTreeR

hasLeafFromList, hasLeafFromListR :: Eq a => a -> [Tree a] -> Bool
hasLeafFromList s xs = hasLeaf s (treeFromList xs)
hasLeafFromListR s xs =
  foldr (\x y -> go $ NodeF (cata go x) y) False xs
  where
    go EmptyF = False
    go (LeafF x) = x == s
    go (NodeF a b) = a || b

inspect $ 'hasLeafFromList === 'hasLeafFromListR

cataFromListMap,cataFromListMapR :: (TreeF a b -> b) -> (c -> Tree a) -> [c] -> b
cataFromListMap go f xs = cata go (treeFromList (map f xs))
cataFromListMapR go f xs =
  foldr (\x y -> go $ NodeF (cata go (f x)) y) (go EmptyF) xs

inspect $ 'cataFromListMap === 'cataFromListMapR

cataFmapList,cataFmapListR :: (TreeF a b -> b) -> (c -> Tree a) -> [Tree c] -> b
cataFmapList  go f xs = cata go (bind (treeFromList xs) f)
cataFmapListR go f xs =
  foldr (\x y -> go $ NodeF (cata go (bind x f)) y) (go EmptyF) xs

inspect $ 'cataFmapList === 'cataFmapListR

compoLeftRight,compoLeftRightR :: (TreeF a b -> b) -> (c -> Tree a) -> (d -> Tree c) -> [d] -> b
compoLeftRight  go f g xs = cata go (bind (treeFromList (map g xs)) f)
compoLeftRightR go f g xs =
  foldr (\x y -> go $ NodeF (cata go (bind (g x) f)) y) (go EmptyF) xs

inspect $ 'compoLeftRight === 'compoLeftRightR

treeFromListMap, treeFromListMapR :: (a -> Tree b) -> [a] -> Tree b
treeFromListMap  f xs = treeFromList (map f xs)
treeFromListMapR f xs = foldr (\x y -> fix (NodeF (f x) y)) empty xs

inspect $ 'treeFromListMap === 'treeFromListMapR

treeFromListAlone, treeFromListAloneR :: [Tree a] -> Tree a
treeFromListAlone  xs = buildR (\g -> foldr (\x -> g . NodeF (cata g x)) (g EmptyF) xs)
treeFromListAloneR xs = foldr node empty xs

inspect $ 'treeFromListAlone === 'treeFromListAloneR
