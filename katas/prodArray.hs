module Main where

import Data.List

prodArray :: [Int] -> [Int]
prodArray lst = case zCount of
    0      -> map (div prod) lst
    1      -> map (\n -> if n == 0 then prod else 0) lst
    zCount -> replicate (length lst) 0
  where zCount  = length $ fst lParted
        prod = product $ snd lParted
        lParted = partition (== 0) lst


main :: IO ()
main = do
  print $ prodArray [0,0,2]
  print $ prodArray [1,2,3]
  print $ prodArray [-1,-2,3]
  print $ prodArray $ take 10 $ cycle [-1,0,3,4,5,6,7,8,9]
  print $ prodArray $ take 10 $ cycle [-1,0,0,4,5,6,7,8,9]
  print $ prodArray $ take 20 $ cycle [-1,2,3,4,5,6,7,8,9]
  print $ prodArray $ take 3000 $ cycle [-1,0,0,4,5,6,7,8,9]
