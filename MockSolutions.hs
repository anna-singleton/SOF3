module Mock where
import Data.Char

type Predicate a = a -> Bool

-- Q1i

testP :: Bool
testP = (isPass [] == False) &&
  (isPass [2, 5, 6, 7] == False) &&
  (isPass [12, 15, 16, 17] == True) &&
  (isPass [5, 8, 12, 15] == True)

isPass :: [Int] -> Bool
isPass = ((<=) 40) . foldr (+) 0


-- Q1ii

testAl :: Bool
testAl = (isAlphabet "" == True) &&
  (isAlphabet "hello!" == False) &&
  (isAlphabet "hello" == True) &&
  (isAlphabet "Hello" == True) &&
  (isAlphabet "SOF3" == False) &&
  (isAlphabet "Software" == True)

isAlphabet :: String -> Bool
isAlphabet = all isAlpha

-- Q1iii

testcmbList :: Bool
testcmbList =
  (cmbList ['s'] "comp" == "cs") &&
  (cmbList "otae" "sfwr" == "software") &&
  (cmbList [1, 3, 5] [0, 2, 4, 6] == [0, 1, 2, 3, 4, 5]) &&
  (cmbList ["1", "2", "3"] ["THE", "SOF", "SYS"] == ["THE", "1", "SOF", "2", "SYS", "3"])

cmbList :: [a] -> [a] -> [a]
cmbList xs ys = concat (zipWith (\ x y -> [y,x]) xs ys)

-- Q1iv

testcmbProd :: Bool
testcmbProd =
  (cmbProd [] [5, 6] == []) &&
  (cmbProd [2, 3, 4] [5, 6] == [10, 18]) &&
  (cmbProd [0.23, 3.4, 7.88, 9.21] [3.4, 5] == [0.782, 17.0]) &&
  (cmbProd [0.23, 3.4, 7.88, 2*0.3] [3.4, 1.3, 2.1, 2] == [0.782, 4.42, 16.548000000000002, 1.2])

cmbProd :: Num a => [a] -> [a] -> [a]
cmbProd = zipWith (*)

-- Q1v

testsqDiff :: Bool
testsqDiff =
  (sqDiff [] == []) &&
  (sqDiff [4, 6] == []) &&
  (sqDiff [6, 4] == [4]) &&
  (sqDiff [4, 6, 3, 1, 8] == [9, 4])

sqDiff :: (Num a, Ord a) => [a] -> [a]
sqDiff [] = []
sqDiff [_] = []
sqDiff (x:y:r) | x > y = (x-y)^2 : sqDiff (y:r)
               | otherwise = sqDiff (y:r)

-- Q1vi

testM2int :: Bool
testM2int =
  (maybe2int [] == 0) &&
  (maybe2int [Just 23] == 23) &&
  (maybe2int [Nothing] == 0) &&
  (maybe2int [Just 2, Nothing, Just 3, Just 16, Nothing] == 21)

maybe2int :: [Maybe Int] -> Int
maybe2int = foldr (+) 0 . map f 
  where f Nothing = 0
        f (Just x) = x

-- Q1vii

testcmb :: Bool
testcmb =
  (cmb "Prod" [2, 3] [] == []) &&
  (cmb "List" [] [2, 3] == []) &&
  (cmb "Prod" [2, 3, 4] [5, 6] == [10, 18]) &&
  (cmb "List" [2, 3, 4] [5, 6] == [5, 2, 6, 3]) &&
  (cmb "Haskell" [2, 3, 4] [5, 6] == []) &&
  (cmb "Prod" [2, 3, 4] [5, 6, 7] == [10, 18, 28]) &&
  (cmb "List" [2, 3, 4] [5, 6, 7] == [5, 2, 6, 3, 7, 4])

cmb :: Num a => String -> [a] -> [a] -> [a]
cmb "Prod" xs ys = zipWith (*) xs ys
cmb "List" xs ys = concat (zipWith (\ x y -> [y,x]) xs ys)
cmb _ _ _ = []


-- Q1viii

{-
# disclaimer:
i did not do this section in the exam, therefore it is likely i have made a mistake
with transcribing a type or something here. please tell me if so, and i will amend it
-}


data CS1 = Student {name :: String, the1, sof1, sys1 :: Int}

s1Db :: [CS1]
s1Db = [Student {name = "Beth", the1 = 65, sof1 = 58, sys1 = 79},
       Student {name = "Adam", the1 = 55, sof1 = 68, sys1 = 61},
       Student {name = "Lisa", the1 = 60, sof1 = 72, sys1 = 65},
       Student {name = "Will", the1 = 71, sof1 = 52, sys1 = 49},
       Student {name = "Mark", the1 = 67, sof1 = 78, sys1 = 50}]

-- Q1viiia

test1MK :: Bool
test1MK = the1Mk s1Db == [("Beth", 65), ("Adam", 55), ("Lisa", 60), ("Will", 71), ("Mark", 67)]

the1Mk :: [CS1] -> [(String, Int)]
the1Mk = map (\ x -> (name x, the1 x))

-- Q1viiib

testSOF1 :: Bool
testSOF1 = tSOF1 s1Db == [("Lisa", 65, 60), ("Mark", 50, 67)]

tSOF1 :: [CS1] -> [(String, Int, Int)]
tSOF1 [] = []
tSOF1 (x:xs) | sof1 x > 70 = ((\ y -> (name y, sys1 y, the1 y)) x) : tSOF1 xs
             | otherwise = tSOF1 xs

-- Q1viiic

testavg :: Bool
testavg = avgSYS1 s1Db == 60.8

avgSYS1 :: [CS1] -> Float
avgSYS1 = (\x -> x / 5) . fromIntegral . foldr ((+) . (\x -> sys1 x)) 0

-- Q1ix

-- wtf why was this test so fucking long

testvowelDigit :: Bool
testvowelDigit =
  (vowelDigit "" == False) &&
  (vowelDigit "a2" == True) &&
  (vowelDigit "aa22" == False) &&
  (vowelDigit "a2a2" == True) &&
  (vowelDigit "b2b2" == False) &&
  (vowelDigit "a2a21" == False) &&
  (vowelDigit "2a4o" == False) &&
  (vowelDigit "a2ab2" == False) &&
  (vowelDigit "a2o5u8A0" == True) &&
  (vowelDigit "b2o5u8A0" == False)

vowelDigit :: String -> Bool
vowelDigit [] = True
vowelDigit (_:[]) = True
vowelDigit (v:d:r) | isVowel v && isNumber d = vowelDigit r
                   | otherwise = False
  where isVowel c =
          c == 'a' || c == 'A' ||
          c == 'e' || c == 'E' ||
          c == 'i' || c == 'I' ||
          c == 'o' || c == 'O' ||
          c == 'u' || c == 'U'
          

-- vowelDigit :: String -> Bool
-- vowelDigit [] = True
-- vowelDigit (_:[]) = True
-- vowelDigit (v:d:r) | v `elem` "aAeEiIoOuU" && isNumber d = vowelDigit r
--                    | otherwise = False

-- Q1x

data BinTree x = Lf Int | Branch (BinTree x) x (BinTree x)
  deriving (Eq, Show)

-- Q1xa

nullBR :: BinTree Int
nullBR = Lf 0

-- Q1xb

testBal :: Bool
testBal =
  (isTreeBal (nullBR) == True) &&
  (isTreeBal (Branch (Lf 1) 1 (Branch (Lf 2) 2 (Branch (Lf 3) 3 (Lf 3)))) == False) &&
  (isTreeBal (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == True)

isTreeBal :: BinTree a -> Bool
isTreeBal (Lf _) = True
isTreeBal (Branch a _ b) = isTreeBal a && isTreeBal b && getDepth a == getDepth b
  where getDepth (Lf x) = x
        getDepth (Branch c _ d) = max (getDepth c) (getDepth d)

-- Q1xc

testN :: Bool
testN =
  (treeNodes nullBR == 0) &&
  (treeNodes (Branch (Lf 1) 3 (Lf 1)) == 1) &&
  (treeNodes (Branch (Lf 1) 3 (Branch (Lf 2) 8 (Lf 2))) == 2) &&
  (treeNodes (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == 3)

treeNodes :: BinTree a -> Int
treeNodes (Lf _) = 0
treeNodes (Branch a _ b) = 1 + treeNodes a + treeNodes b

-- Q1xi

testBcode :: Bool
testBcode =
  (toBarcode "" == Just "") &&
  (toBarcode "00" == Just "..") &&
  (toBarcode "1111" == Just "||||") &&
  (toBarcode "0010111" == Just "..|.|||") &&
  (toBarcode " " == Nothing)

toBarcode :: String -> Maybe String
toBarcode s | f s = (Just (map g s))
            | otherwise = Nothing
  where f cs = foldr ((&&) . (\x -> x=='0' || x =='1')) True cs
        g '0' = '.'
        g '1' = '|'
