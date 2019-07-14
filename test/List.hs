{-# LANGUAGE TemplateHaskell #-}

module List where

import Prelude hiding (foldr, map)

import Tungsten.Fix
import Tungsten.Structure.List

import Test.Inspection

foldrMap, foldrMapR :: (b -> c -> c) -> c -> (a -> b) -> List a -> c
foldrMap  c n f xs = foldr c n (map f xs)
foldrMapR c n f xs = foldr (c . f) n xs

inspect $ 'foldrMap === 'foldrMapR

fromTo, fromToR :: [a] -> [a]
fromTo  xs = fromList (toList xs)
fromToR xs = xs

inspect $ 'fromTo === 'fromToR

toFrom, toFromR :: List a -> List a
toFrom  xs = toList (fromList xs)
toFromR xs = xs

inspect $ 'toFrom === 'toFromR
