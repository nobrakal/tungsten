{-# LANGUAGE TemplateHaskell #-}

module Fix where

import Tungsten.Fix

import Test.Inspection

cataAna, cataAnaR :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
cataAna  f g x = cata f (ana g x)
cataAnaR f g x = h x where h = f . fmap h . g

inspect $ 'cataAna === 'cataAnaR
