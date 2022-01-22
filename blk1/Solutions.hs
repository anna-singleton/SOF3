module Solutions where

-- Q 5.1

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

greetTest :: Bool
greetTest
  = greet "Kofi" == "Hello Kofi!"
    && greet "Jeremy" == "Hello Jeremy!"
    && greet "" == "Hello !"

-- Q 5.2

cakeBill :: Int -> Int -> String
cakeBill 1 price = "The cost of 1 cake at " ++ show price ++
  "p each is " ++ show price ++ "p."
cakeBill quantity price = "The cost of " ++ show quantity ++
  " cakes at " ++ show price ++ "p each is " ++ show (quantity*price)
  ++ "p."

cakeBillTest :: Bool
cakeBillTest =
  cakeBill 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill 1 3 == "The cost of 1 cakes at 3p each is 3p."
  && cakeBill 2 3 == "The cost of 2 cakes at 3p each is 6p."

-- Q 5.3

bananas :: Int -> Int
bananas order | order < 2 = undefined
              | order <= 50 = (order * 300) + 499
              | otherwise         = (order * 300) + (499 - 150)

bananasTest :: Bool
bananasTest =
  bananas 2 == 1099
  && bananas 20 == 6349


-- Q 5.4

pennies2pounds :: Int -> String
pennies2pounds pennies = show (fromIntegral pennies / 100)


-- Q 5.5


implies :: Bool -> Bool -> Bool -- explicit parameters
implies True True = True
implies True False = False
implies False True = True
implies False False = True

implies_ :: Bool -> Bool -> Bool -- implicit parameters
implies_ a b = (not a) || b

-- implies_ = (||) . not
-- above is implicit parameters

impliesTest :: Bool
impliesTest =
  implies True True == implies_ True True &&
  implies True False == implies_ True False &&
  implies False True == implies_ False True &&
  implies False False == implies_ False False

implies' :: Bool -> Bool -> Bool
implies' True False = False
implies' _ _ = True


-- Q 5.6
data Item = Dog | Chicken | Grain deriving (Eq, Show)

eats :: Item -> [Item]
eats Dog = [Chicken]
eats Chicken = [Grain]
eats Grain = []

danger :: Item -> Item -> Bool
danger i1 i2 = (i1 `elem` (eats i2)) || (i2 `elem` (eats i1))


-- Q 6.1

incList :: [Int] -> [Int]
incList [] = []
incList list = (map (+1) (take 1 list)) ++ incList (drop 1 list)

incList' :: [Int] -> [Int]
incList' list = map (+1) list


-- Q 6.2

greetTest' :: [(String, String)] -> [(String, String, String, Bool)]
greetTest' list = map f (list) where
  f :: (String, String) -> (String, String, String, Bool)
  f pair = if snd pair == (greet (fst pair)) then
    (fst pair, snd pair, greet (fst pair), True)
    else
    (fst pair, snd pair, greet (fst pair), False)


-- Q 6.3

pos :: Eq a => a -> [a] -> Int
pos a [] = 0
pos a list = if a == (head list) then
  0
  else
  1 + pos a (drop 1 list)

-- Q 6.4

-- we assume the sorted list is in ASCENDING order

insert :: Ord a => a -> [a] ->[a]
insert x [] = [x]
insert x list = if x <= head list then
  x : list
  else
  (head list) : (insert x (drop 1 list))

-- Q 6.5

isort :: Ord a => [a] -> [a]
isort list = foldr insert [] list


-- Q 6.6 TODO
{- 
insert' :: Ord a => a -> [a] -> [a]
insert' x = foldr insx x []
  where
    insx = foldr insert x []
-}

-- Q 6.7

mystery :: [a] -> [a]
mystery = foldr (:) []

-- Q 6.8 TODO
{- 
mapAsRF :: (a -> b) -> [a] -> [b]
mapAsRF f = foldr f []
-}

-- Q 6.9

-- foldl (+) 0 [5,7,6,2] ((((0+5)+7)+6)+2)
-- foldr (+) 0 [5,7,6,2] (5+(7+(6+(2+0))))

-- Q 6.10

revRF, revLF :: [a] -> [a]
revRF = foldr undefined undefined
revLF = foldl undefined undefined


-- Q 6.11

lenRF, lenLF :: [a] -> Int
lenRF = foldr undefined undefined
lenLF = foldl undefined undefined
