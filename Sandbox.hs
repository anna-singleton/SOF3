module Sandbox where

cmbProd :: [Int] -> [Int] -> [Int]
cmbProd = zipWith (*)

cmbList :: [a] -> [a] -> [a]
cmbList xs ys = concat (zipWith (\ x y -> [y,x]) xs ys)

data BinTree x = Lf Int | Branch (BinTree x) x (BinTree x)
  deriving (Eq, Show)

testTree :: BinTree Char
testTree = Branch (Branch (Branch (Lf 1) 'd' (Lf 2)) 'b' (Lf 3)) 'a' (Branch (Lf 4) 'c' (Branch (Lf 5) 'g' (Lf 6)))

{-
         a
        / \
       /   \
      b     c
     / \   / \
    d   3 4   g
   / \       / \
  1   2     5   6
-}


isTreeBal :: BinTree a -> Bool
isTreeBal (Lf _) = True
isTreeBal (Branch a _ b) = isTreeBal a && isTreeBal b && getDepth a == getDepth b
  where getDepth (Lf x) = x
        getDepth (Branch c _ d) = max (getDepth c) (getDepth d)


markdist :: Float -> Float
markdist x | x > 40 && x <= 45 = 40 + (x - 40) * 0.5
           | x > 45 && x <= 80 = 45 + (x - 50) * (35 / 45)
           | x > 80 && x <= 95 = 80 + (x - 95) * (20 / 5)
           | otherwise = x
