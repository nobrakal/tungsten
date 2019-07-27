{-# LANGUAGE TemplateHaskell #-}

module Graph where

import Data.Tuple (swap)
import Tungsten.Structure.Graph

import Test.Inspection

transposeSwap, transposeSwapR :: [(a,a)] -> Graph a
transposeSwap  xs = transpose (edges xs)
transposeSwapR xs = edges (map swap xs)

inspect $ 'transposeSwap === 'transposeSwapR
