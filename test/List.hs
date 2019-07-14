{-# LANGUAGE TemplateHaskell #-}

module List where

import Prelude hiding (foldr, map)

import Tungsten.Fix
import Tungsten.Structure.List

import Test.Inspection

foldrMap, foldrMapR :: (a -> b -> b) -> b -> (c -> a) -> List c -> b
foldrMap  c n f xs = foldr c n (map f xs)
foldrMapR c n f xs = foldr (c . f) n xs

inspect $ 'foldrMap === 'foldrMapR
