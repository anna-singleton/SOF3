module P99 where
import Data.List

-- P1

p1 :: [a] -> Maybe a
p1 (x:[]) = (Just x)
p1 (_:xs) = p1 xs
p1 [] = Nothing

-- P2

p2 :: [a] -> Maybe a
p2 (x:_:[]) = (Just x)
p2 (_:xs) = p2 xs
p2 [] = Nothing


-- P3

p3 :: [a] -> Int -> Maybe a
p3 (x:_) 1 = (Just x)
p3 (_:xs) n = p3 xs (n-1)
p3 [] _ = Nothing


-- P4

p4 :: [a] -> Int
p4 [] = 0
p4 (_:xs) = 1 + p4 xs

-- P5

p5 :: [a] -> [a]
p5 [] = []
p5 (x:xs) = (p5 xs) ++ [x]

-- P6

p6 :: Eq a => [a] -> Bool
p6 [] = True
p6 [_] = True
p6 xs | (head xs) == (last xs) = p6 ((init . tail) xs)
      | otherwise = False

-- P7

data NestedList a = Elem a | List [NestedList a]

p7 :: NestedList a -> [a]
p7 (Elem a) = [a]
p7 (List (x:[])) = p7 x
p7 (List (x:xs)) = (p7 x) ++ p7 (List xs)
p7 (List []) = []

-- P8

p8 :: Eq a => [a] -> [a]
p8 [] = []
p8 [x] = [x]
p8 (x:xs) | x == (head xs) = (p8 xs)
          | otherwise = x : (p8 xs)

-- P9

p9 :: Eq a => [a] -> [[a]]
p9 = group 

-- P10

p10 :: Eq a => [a] -> [(Int, a)]
p10 = map (\x -> (length x, head x)) . group

-- P11

data Item a = Single a | Multiple Int a
  deriving (Eq, Show)

p11 :: Eq a => [a] -> [Item a]
p11 = map f . group
  where f x | length x == 1 = Single (head x)
            | otherwise = Multiple (length x) (head x)

-- P12

p12 :: [Item a] -> [a]
p12 = concat . map f 
  where f (Single x) = [x]
        f (Multiple c x) = take c $ repeat x

-- P13


-- P14

p14 :: [a] -> [a]
p14 [] = []
p14 (x:xs) = x : x : p14 xs

-- P15

p15 :: [a] -> Int -> [a]
p15 _ 0 = []
p15 [] _ = []
p15 (x:xs) n = x : (p15 [x] (n-1)) ++ (p15 xs n)

-- P16

p16 :: [a] -> Int -> [a]
p16 xs c | length xs < c = xs
         | otherwise =  take (c-1) xs ++ (p16 (drop c xs) c)

-- P17

p17 :: [a] -> Int -> ([a], [a])
p17 = flip splitAt
