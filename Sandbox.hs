module Sandbox where

cmbProd :: [Int] -> [Int] -> [Int]
cmbProd = zipWith (*)

cmbList :: [a] -> [a] -> [a]
cmbList xs ys = concat (zipWith (\ x y -> [y,x]) xs ys)
