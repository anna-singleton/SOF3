module Mock where

type Predicate a = a -> Bool

-- Q1i

testP :: Bool
testP = (isPass [] == False) &&
  (isPass [2, 5, 6, 7] == False) &&
  (isPass [12, 15, 16, 17] == True) &&
  (isPass [5, 8, 12, 15] == True)

isPass :: [Int] -> Bool
isPass = undefined


-- Q1ii

testAl :: Bool
testAl = (isAlphabet "" == True) &&
  (isAlphabet "hello!" == False) &&
  (isAlphabet "hello" == True) &&
  (isAlphabet "Hello" == True) &&
  (isAlphabet "SOF3" == False) &&
  (isAlphabet "Software" == True)

isAlphabet :: String -> Bool
isAlphabet = undefined

-- Q1iii

testcmbList :: Bool
testcmbList =
  (cmbList ['s'] "comp" == "cs") &&
  (cmbList "otae" "sfwr" == "software") &&
  (cmbList [1, 3, 5] [0, 2, 4, 6] == [0, 1, 2, 3, 4, 5]) &&
  (cmbList ["1", "2", "3"] ["THE", "SOF", "SYS"] == ["THE", "1", "SOF", "2", "SYS", "3"])

cmbList :: [a] -> [a] -> [a]
cmbList = undefined

-- Q1iv

testcmbProd :: Bool
testcmbProd =
  (cmbProd [] [5, 6] == []) &&
  (cmbProd [2, 3, 4] [5, 6] == [10, 18]) &&
  (cmbProd [0.23, 3.4, 7.88, 9.21] [3.4, 5] == [0.782, 17.0]) &&
  (cmbProd [0.23, 3.4, 7.88, 2*0.3] [3.4, 1.3, 2.1, 2] == [0.782, 4.42, 16.548000000000002, 1.2])

cmbProd :: Num a => [a] -> [a] -> [a]
cmbProd = undefined

-- Q1v

testsqDiff :: Bool
testsqDiff =
  (sqDiff [] == []) &&
  (sqDiff [4, 6] == []) &&
  (sqDiff [6, 4] == [4]) &&
  (sqDiff [4, 6, 3, 1, 8] == [9, 4])

sqDiff :: (Num a, Ord a) => [a] -> [a]
sqDiff = undefined

-- Q1vi

testM2int :: Bool
testM2int =
  (maybe2int [] == 0) &&
  (maybe2int [Just 23] == 23) &&
  (maybe2int [Nothing] == 0) &&
  (maybe2int [Just 2, Nothing, Just 3, Just 16, Nothing] == 21)

maybe2int :: [Maybe Int] -> Int
maybe2int = undefined

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
cmb = undefined


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
the1Mk = undefined

-- Q1viiib

testSOF1 :: Bool
testSOF1 = tSOF1 s1Db == [("Lisa", 65, 60), ("Mark", 50, 67)]

tSOF1 :: [CS1] -> [(String, Int, Int)]
tSOF1 = undefined

-- Q1viiic

testavg :: Bool
testavg = avgSYS1 s1Db == 60.8

avgSYS1 :: [CS1] -> Float
avgSYS1 = undefined

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
vowelDigit = undefined


-- Q1x

data BinTree x = Lf Int | Branch (BinTree x) x (BinTree x)
  deriving (Eq, Show)

-- Q1xa

nullBR = undefined

-- Q1xb

testBal :: Bool
testBal =
  (isTreeBal (nullBR) == True) &&
  (isTreeBal (Branch (Lf 1) 1 (Branch (Lf 2) 2 (Branch (Lf 3) 3 (Lf 3)))) == False) &&
  (isTreeBal (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == True)

isTreeBal :: BinTree a -> Bool
isTreeBal = undefined

-- Q1xc

testN :: Bool
testN =
  (treeNodes nullBR == 0) &&
  (treeNodes (Branch (Lf 1) 3 (Lf 1)) == 1) &&
  (treeNodes (Branch (Lf 1) 3 (Branch (Lf 2) 8 (Lf 2))) == 2) &&
  (treeNodes (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == 3)

treeNodes :: BinTree a -> Int
treeNodes = undefined

-- Q1xi

testBcode :: Bool
testBcode =
  (toBarcode "" == Just "") &&
  (toBarcode "00" == Just "..") &&
  (toBarcode "1111" == Just "||||") &&
  (toBarcode "0010111" == Just "..|.|||") &&
  (toBarcode " " == Nothing)

toBarcode :: String -> Maybe String
toBarcode = undefined
