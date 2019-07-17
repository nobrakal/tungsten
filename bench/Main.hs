{-# Language TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Tungsten.Fix
import qualified Tungsten.Structure.List as L

import Criterion.Main
import Control.DeepSeq

import Prelude hiding (sum)

instance NFData a => NFData (L.List a) where
  rnf (Fix x) =
    case x of
      L.NilF -> ()
      L.ConsF y ys -> rnf y `seq` rnf ys

intList :: Int -> [Int]
intList x = [1..x]

intListL :: Int -> L.List Int
intListL = L.range 0

sum :: [Int] -> Int
sum = foldr (+) 0

sumL :: L.List Int -> Int
sumL = L.foldr (+) 0
{-# INLINE sumL #-}

main :: IO ()
main =
  defaultMain
  [ bgroup "Prelude lists"
    [ bench "100" $ nf (sum . map (+1)) $!! intList 100
    , bench "1000" $ nf (sum . map (+1)) $!! intList 1000
    , bench "10000" $ nf (sum . map (+1)) $!! intList 10000
    ]
  , bgroup "Fixed lists"
    [ bench "100" $ nf (sumL . L.map (+1)) $!! intListL 100
    , bench "1000" $ nf (sumL . L.map (+1)) $!! intListL 1000
    , bench "10000" $ nf (sumL . L.map (+1)) $!! intListL 10000
    ]
  ]
