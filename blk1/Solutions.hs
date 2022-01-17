module Solutions where

-- Question 1

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

greetTest :: Bool
greetTest
  = greet "Kofi" == "Hello Kofi!"
    && greet "Jeremy" == "Hello Jeremy!"
    && greet "" == "Hello !"

-- Question 2

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

-- Question 3

bananas :: Int -> Int
bananas order | order < 2 = undefined
              | order <= 50 = (order * 300) + 499
              | otherwise         = (order * 300) + (499 - 150)

bananasTest :: Bool
bananasTest =
  bananas 2 == 1099
  && bananas 20 == 6349


-- Question 4

pennies2pounds :: Int -> String
pennies2pounds pennies = show (fromIntegral pennies / 100)


-- Question 5


implies :: Bool -> Bool -> Bool -- explicit parameters
implies True True = True
implies True False = False
implies False True = True
implies False False = True

implies_ :: Bool -> Bool -> Bool -- implicit parameters
implies_ a b = (not a) || b

impliesTest :: Bool
impliesTest =
  implies True True == implies_ True True &&
  implies True False == implies_ True False &&
  implies False True == implies_ False True &&
  implies False False == implies_ False False
