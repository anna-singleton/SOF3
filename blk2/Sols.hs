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
ePbs2iM x | x==Nothing = Nothing
          | parity (fromJust x) /= True = Nothing
          | otherwise = Just (ePbs2i (fromJust x))

doubleOdd :: Int -> Int
doubleOdd x | odd x = x*2

doubleOddM :: Maybe Int -> Maybe Int
doubleOddM Nothing = Nothing
doubleOddM x | odd (fromJust x) = Just ((fromJust x) *2)
             | otherwise = Nothing
             
doubleOddM'' :: Maybe Int -> Maybe Int
doubleOddM'' Nothing = Nothing
doubleOddM'' (Just x) | odd x = Just (x*2)
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

-- Q3.3 TODO ask about fix

fix :: (a->a) -> a
fix    f       = f (fix f)

ones' :: [Integer]
ones' = fix (1:)

nats' :: [Integer]
nats' = fix (\x -> 0 : map (+1) x)

-- Q3.5

findPlateau :: Eq a => [a] -> a
findPlateau (x1:x2:xs) | x1==x2 = x1
                       | otherwise = findPlateau (x2:xs)

tz :: Int -> Int
tz    n = negate (n `div` 2)

mystery3_5 :: Eq a => (a -> a) -> a -> a
mystery3_5 = (findPlateau .) . iterate


-- Q3.6

mersenne :: [Int]
mersenne = [x | x <- [0..], f x 1]
  where f y p | (2^p) <= y = f y (p+1)
              | (2^p) == (y+1) = True
              | otherwise = False

mersenne' :: [Int]
mersenne' = [(2^x) - 1 | x <- [1..]]

eratosthenes :: [Int]
eratosthenes = sieve (map Just [2..])
  where
    sieve (Just n  : ns) = n : sieve (zipWith ($) mark_n ns)
      where mark_n = cycle (replicate (n-1) id ++ [const Nothing]) 
    sieve (Nothing : ns) = sieve ns


mersennePrime :: [Int]
mersennePrime = [x | x <- [0..], (f x mersenne') && (f x eratosthenes)]
  where f x (l:ls) | x > l = f x ls
                   | x == l = True
                   | otherwise = False

-- go up the lists of mersenne and prime, and stop checking a list if
-- x == the current or x > the current. If x is in both lists, accept it


















  
-- Q4: Lazy Trees

newtype Tree a = Tree [(a, Tree a)] deriving Show

data VM_Event = Coin | Choc | Fudj | Halt deriving (Eq, Show)

type VM = Tree VM_Event

vm1_2 :: VM
vm1_2  = Tree [(Coin, Tree [(Choc, Tree [(Coin, Tree [(Choc, Tree [])])])])]

vm1_e :: VM
vm1_e = Tree [(Coin, Tree [(Choc, vm1_e)])]

stop :: Tree a
stop = Tree []

leadsto :: a -> Tree a -> Tree a
e `leadsto` b = Tree [(e, b)]

branch :: Tree a -> Tree a -> Tree a
Tree ts `branch` Tree us = Tree (ts ++ us)

vm1_2' :: VM
vm1_2' = Coin `leadsto` (Choc `leadsto` (Coin `leadsto` (Choc `leadsto` stop)))

vm1_e' :: VM
vm1_e' = Coin `leadsto` (Choc `leadsto` vm1_e')

vm1_h :: VM
vm1_h = (Coin `leadsto` ((Choc `leadsto` vm1_h)
                         `branch`
                         (Halt `leadsto` stop)))
        `branch`
        (Halt `leadsto` stop)

vm1_h' :: VM
vm1_h' = (Coin `leadsto` (Choc `leadsto` vm1_h))
         `branch`
         (Halt `leadsto` stop)

-- newtype Tree a = Tree [(a, Tree a)] deriving Show
-- Q4.3
takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = (Tree [])  
takeTree _ (Tree []) = (Tree [])
--takeTree d (Tree ts) = (Tree (map (takeTree (d-1) . f) ts))
  --where f t = snd t

-- this handles only the case where the trees do not branch
takeTree d (Tree ((x,y):_)) = (Tree [(x, (takeTree (d-1) y))])


-- takeTree' :: Int -> Tree a -> Tree a
-- takeTree' 0 _ = (Tree [])  
-- takeTree' _ (Tree []) = (Tree [])
-- takeTree' d (Tree ts) = (Tree (map (f d) ts))
--   where f d (x,y) = (Tree [(x, (takeTree (d-1) y))])

-- Q4.4
  

  
-- Q5

{-
tic tac toe
-}

newtype Grid = Grid (Int -> Int)
-- 0 is not played
-- 1 is p1 (Os)
-- 2 is p2 (Xs)

f :: Int -> Int
f x = x+1

g :: Int -> Int
g x = x * 2

h :: Int -> Int
h = f . g
