{-# LANGUAGE TemplateHaskell #-}

module Fix where

import Tungsten.Fix

import Test.Inspection

cataAna, cataAnaR :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
cataAna  f g x = cata f (ana g x)
cataAnaR f g x = h x where h = f . fmap h . g

inspect $ 'cataAna === 'cataAnaR

cataFix, cataFixR :: Functor f => Fix f -> Fix f
cataFix  x = cata Fix x
cataFixR x = x

inspect $ 'cataFix === 'cataFixR
