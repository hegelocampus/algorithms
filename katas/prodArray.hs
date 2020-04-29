module Main where

import Data.List
import Criterion.Main

prodArray :: [Int] -> [Int]
prodArray lst = case zCount of
    0      -> map (div prod) lst
    1      -> map (\n -> if n == 0 then prod else 0) lst
    zCount -> replicate (length lst) 0
  where zCount  = length $ fst lParted
        prod = product $ snd lParted
        lParted = partition (== 0) lst


main = defaultMain [
  bgroup "prodArray"
      [ bench "25 with one 0" $ whnf prodArray (take 25 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "25 with two 0s" $ whnf prodArray (take 25 $ cycle [-1,0,0,4,5,6,7,8,9])
      , bench "50 with one 0" $ whnf prodArray (take 50 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "50 with two 0s" $ whnf prodArray (take 50 $ cycle [-1,0,0,4,5,6,7,8,9])
      , bench "100 with one 0" $ whnf prodArray (take 100 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "100 with two 0s" $ whnf prodArray (take 100 $ cycle [-1,0,0,4,5,6,7,8,9])
      , bench "200 with one 0" $ whnf prodArray (take 200 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "200 with two 0s" $ whnf prodArray (take 200 $ cycle [-1,0,0,4,5,6,7,8,9])
      , bench "500 with one 0" $ whnf prodArray (take 500 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "500 with two 0s" $ whnf prodArray (take 500 $ cycle [-1,0,0,4,5,6,7,8,9])
      , bench "1000 with one 0" $ whnf prodArray (take 1000 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "1000 with two 0s" $ whnf prodArray (take 1000 $ cycle [-1,0,0,4,5,6,7,8,9])
      , bench "2000 with one 0" $ whnf prodArray (take 2000 $ cycle [-1,0,2,4,5,6,7,8,9])
      , bench "2000 with two 0s" $ whnf prodArray (take 2000 $ cycle [-1,0,0,4,5,6,7,8,9])
      ]
  ]
