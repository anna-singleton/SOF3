module Sols where

import Data.Maybe

-- Q1.1

oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  n `mod` 2 == 0 ]
oneone' [] = []
oneone' (n:ns) | n `mod` 2 == 0 = (n+1) : oneone' ns
               | otherwise = oneone' ns

-- Q1.2

onetwo, onetwo' :: [String] -> [Bool]
onetwo css = [(fromEnum c) `mod` 2 == 1 |
              cs <- css, length cs > 1, c <- cs, c `elem` ['a'..'z']]

onetwo' [] = []
onetwo' (x:xs) = (f x) ++ onetwo' xs
     where f [] = []
           f (y:ys) = ((fromEnum y) `mod` 2 == 1) : f ys

-- Q1.3

bitstring2int :: [Bool] -> Int
bitstring2int = foldl (\ n b -> 2*n + if b then 1 else 0) 0

parity :: [Bool] -> Bool
parity = (==0) . (`mod` 2) . length . filter (== True) 

onethree, onethree' :: [[Bool]] -> [Int]
onethree = map bitstring2int . filter parity
onethree' xs = [bitstring2int x | x <- xs, parity x]

--Q2.1

ePbs2i :: [Bool] -> Int
ePbs2i bs | parity bs = bitstring2int bs 

{-
evaluating ePbs2i [True, False, True, False] gives us 10
evaluating ePbs2i [True, False, False, False] gives us an exception
-}
          
-- Q2.2


ePbs2iM :: Maybe [Bool] -> Maybe Int
--ePbs2iM x = case x of
  --Nothing -> Nothing
  --y -> Just(ePbs2i (fromJust y))
ePbs2iM x | x==Nothing = Nothing
          | parity (fromJust x) /= True = Nothing
          | otherwise = Just (ePbs2i (fromJust x))

doubleOdd :: Int -> Int
doubleOdd x | odd x = x*2

doubleOddM :: Maybe Int -> Maybe Int
doubleOddM Nothing = Nothing
doubleOddM x | odd (fromJust x) = Just ((fromJust x) *2)
             | otherwise = Nothing
             
maybeDouble :: Maybe Int -> Maybe Int
maybeDouble x = maybe Nothing (Just . (*2)) x

             
doubleOddM'   :: Maybe Int -> Maybe Int
doubleOddM' x = maybe Nothing (f) x
 where f y | odd y = Just (y * 2)
           | otherwise = Nothing


-- Q 2.3

type Error a = Either String a

ePbs2iE :: Error [Bool] -> Error Int
ePbs2iE (Left msg) = Left msg
ePbs2iE (Right bs) | parity bs = Right (bitstring2int bs)
                   | otherwise = Left "input has odd priority"

doubleOddE :: Error Int -> Error Int
doubleOddE (Right x) | odd x = (Right (x * 2))
                     | otherwise = (Left "ERROR: input not odd")
doubleOddE (Left msg) = Left msg

-- Q3.1

ones, nats :: [Integer]
ones = 1 : ones
nats = 0 : map succ nats

-- Q3.2

{-
    ones:
base = 1
recursive = ones ++ [1]

    nats:
base = 0
recursive = nats ++ [last+1]
-}

-- Q3.3

fix :: (a->a) -> a
fix    f       = f (fix f)

ones' :: [Integer]
ones' = fix (1:)

