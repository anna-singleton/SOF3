module ResitPaper where
import Data.Char
import Data.List

{-
#Q1i
-}

allSoft3 :: [Char] -> Bool
allSoft3 = all (flip elem "Software3")

allTest :: Bool
allTest =
  allSoft3 "" == True &&
  allSoft3 " " == False &&
  allSoft3 "oft3w" == True &&
  allSoft3 "ofT3w" == False &&
  allSoft3 "free" == True &&
  allSoft3 "Software" == True


{-
#Q1ii
-}

three4th :: Fractional a => a -> a
three4th = (/4) . (*3)

testThreeq :: Bool
testThreeq =
  three4th 0 == 0.0 &&
  three4th 1 == 0.75 &&
  three4th 8.0 == 6.0 &&
  three4th 4.8 == 3.5999999999999996 &&
  three4th (-8) == -6.0


{-
#Q1iii
-}

testSum :: Bool
testSum =
  sqSum [] == 0 &&
  sqSum [1, 3, 4] == 26 &&
  sqSum [1, 3, 4.0] == 26.0 &&
  sqSum [1, 3, 4.2] == 27.64


sqSum :: Num a => [a] -> a
sqSum = foldr ((+) . (^2)) 0

{-
#Q1iv
-}

same2other :: Eq a => [a] -> Bool
same2other xs | length xs < 3 = False
same2other xs@(a:b:ys)
  | a /= b = False
  | otherwise = a `elem` ys

testsame2other :: Bool
testsame2other = (same2other "" == False) &&
    (same2other "a" == False) &&
    (same2other [2] == False) &&
    (same2other "aaa" == True) &&
    (same2other [8, 8, 8] == True) &&
    (same2other [2, 2, 3, 4] == False) &&
    (same2other [2, 2, 3, 4, 2] == True) &&
    (same2other "aatdaya" == True) &&
    (same2other "aatdgyb" == False) &&
    (same2other [8, 8, 3, 8, 3, 6, 8, 12] == True)

{-
#Q1v
-}

testVowels :: Bool
testVowels =
  justVowels "Hello world!" == "eoo" &&
  justVowels "Aaron562qe" == "Aaoe" &&
  justVowels "sof3isGREATsoenjOY" == "oiEAoeO" &&
  justVowels "numberPLATE2021" == "ueAE"
  
justVowels :: String -> String
justVowels = filter (flip elem "aeiouAEIOU")


{-
#Q1vi
-}

testRev :: Bool
testRev =
    revAllLower "" == "" &&
    revAllLower "!reTupmoC" == "computer!" &&
    revAllLower "Software3" == "3erawtfos" &&
    revAllLower "Software3!" == "!3erawtfos" &&
    (revAllLower $ revAllLower "Software3!") == "software3!"

revAllLower :: String -> String
revAllLower = reverse . map toLower


{-
#Q1vii
-}

testfindPlurals :: Bool
testfindPlurals =
    (findPlurals "" == "") &&
    (findPlurals "THE1SOF1" == "1") &&
    (findPlurals "accommodation" == "acmo") &&
    (findPlurals "Accommodation" == "cmo") &&
    (findPlurals "THE2SOF2SYS1DAT1HCI1" == "12HST") &&
    (findPlurals [1, 3, 4, 2, 3, 5, 7, 1, 9, 3] == [1,3]) &&
    (findPlurals [1, 3, 4, 2, 3, 5, 7, 1, 9, 5] == [1,3,5]) &&
    (findPlurals [1, 5, 4, 2, 3, 5, 7, 1, 9, 3] == [1,3,5])

findPlurals :: Ord a => [a] -> [a]
findPlurals = map head . filter ((>1) . length) . group . sort

{-
#Qviii
-}

data Course = NICE | EASY | SOFT | HARD | HALF | FULL deriving (Show, Eq)


data Student = Student SName Age College CMark
data College = Halifax | James | Langwith deriving (Show, Eq)
type SName = String
type Mark = Int
type Age = Int
type CMark = [(Course, Double)]
benWalker, jBond, yWu, kSong, mGove :: Student
benWalker = Student "Ben Walker" 19 Halifax [(SOFT, 62), (EASY, 42), (FULL, 62)]
jBond = Student "James Bond" 21 Halifax [(SOFT, 42), (EASY, 42)]
mGove = Student "Mike Gove" 21 Halifax [(SOFT, 22), (EASY, 42)]
yWu = Student "Yang Wu" 18 Halifax [(SOFT, 22)]
kSong = Student "Kelly Song" 22 Halifax []

testPrereqs :: Bool
testPrereqs =
    (checkPrereqs benWalker == False) &&
    (checkPrereqs jBond == True) &&
    (checkPrereqs yWu == True) &&
    (checkPrereqs mGove == False) &&
    (checkPrereqs kSong == True)

checkPrereqs :: Student -> Bool
checkPrereqs s@(Student _ _ _ c) = all (hasPR s) (getAllPRs c)

getAllPRs :: CMark -> [Course]
getAllPRs = nub . concat . map (getPRs . fst)
  where
    getPRs :: Course -> [Course]
    getPRs EASY = [SOFT]
    getPRs HARD = [EASY, NICE]
    getPRs FULL = [SOFT, HALF]
    getPRs _ = []

hasPR :: Student -> Course -> Bool
hasPR (Student _ _ _ c) x = 40 <= maybe 0 id (lookup x c)

{-
#Q1ix
-}

testND :: Bool
testND =
    numDiff "soft" == 1 &&
    numDiff "soft2" == 2 &&
    numDiff "soft3" == -2 &&
    numDiff "char27481" == 56 &&
    numDiff "3to15is117" == -17 &&
    numDiff "some2743367numbers" == 28


numDiff :: String -> Int
numDiff xs = (product $ filter even nums) - (sum $ filter odd nums)
  where
    nums :: [Int]
    nums = map digitToInt $ filter isDigit xs

{-
#Q1x
-}

data CSStudent = CSStudent {sid :: SID, sname :: SName, stage :: Stage}
    deriving (Ord, Eq, Show)

type SID = Int

data Stage = One | Two | Three | Four deriving (Ord, Eq, Show)

data DBTreeO a = DBEmp Int | DBLeafO Int [a] | DBNodeO Int [a] [DBTreeO a]
    deriving (Eq, Show)

csStud, students, csYork, dbThree, stdTest :: DBTreeO CSStudent
students = DBLeafO 4 [
  CSStudent {sid = 2, sname = "Mark Foster", stage = One},
  CSStudent {sid = 3, sname = "Juli Smith", stage = Two}]
stdTest = DBLeafO 4 [
  CSStudent {sid = 1, sname = "Jane Hay", stage = One},
  CSStudent {sid = 3, sname = "Mat Bell", stage = Three}]
csStud = DBEmp 4
csYork = DBLeafO 3 [
  CSStudent {sid = 2, sname = "Mark Foster", stage = One},
  CSStudent {sid = 3, sname = "Juli Smith", stage = Two}]
dbThree = DBNodeO 3 [CSStudent {sid = 3, sname = "Kriss Wells", stage = One}] [DBLeafO 3 [CSStudent {sid = 2, sname = "Sally Hodge", stage = Two}], DBLeafO 3 [CSStudent {sid = 7, sname = "Greg Ovett", stage = Two}, CSStudent {sid = 20, sname = "Ann Webb", stage = Four}]]

{-
#Q1xa
-}

testcsFind :: Bool
testcsFind =
    csFind students 1 == False &&
    csFind students 2 == True &&
    csFind students 3 == True &&
    csFind students 0 == False &&
    csFind students (-13) == False

csFind :: DBTreeO CSStudent -> SID -> Bool
csFind (DBEmp _) _ = False
csFind (DBLeafO _ students) s = any (==s) (map sid students)
csFind (DBNodeO _ students trees) s = (any (==s) (map sid students)) || (any (flip csFind s) trees)

{-
#Q1xb
-}

csInsert :: DBTreeO CSStudent -> CSStudent -> DBTreeO CSStudent
csInsert (DBEmp x) s = DBLeafO x [s]
csInsert y@(DBLeafO x ss) s
  | sid s >= x = (DBLeafO x (sortOn sid ss))
  | not $ csFind y (sid s)= (DBLeafO x (sortOn sid ([s]++ss)))
  | otherwise = y

testInsert :: Bool
testInsert = csInsert students CSStudent {sid =1, sname = "Yi Wu", stage = Four} == DBLeafO 4 [CSStudent {sid = 1, sname = "Yi Wu", stage = Four}, CSStudent {sid = 2, sname = "Mark Foster", stage = One}, CSStudent {sid = 3, sname = "Juli Smith", stage = Two}] &&
  csInsert csStud CSStudent {sid =1, sname = "Mike Brown", stage = One} == DBLeafO 4 [CSStudent {sid = 1, sname = "Mike Brown", stage = One}] &&
  csInsert stdTest CSStudent {sid = 1, sname = "Yi Wu", stage = Four} == DBLeafO 4 [CSStudent {sid = 1, sname = "Jane Hay", stage = One}, CSStudent {sid = 3, sname = "Mat Bell", stage = Three}] &&
  csInsert (csInsert csStud CSStudent {sid = 1, sname = "Mike Brown", stage = One}) CSStudent {sid=2, sname="Georgia Jones", stage=Two} == DBLeafO 4 [CSStudent {sid = 1, sname = "Mike Brown", stage = One}, CSStudent {sid = 2, sname = "Georgia Jones", stage = Two}] &&
  csInsert (csInsert (csInsert csStud CSStudent {sid = 1, sname = "Mike Brown", stage = One}) CSStudent {sid=2, sname="Georgia Jones", stage=Two}) CSStudent {sid=3, sname="Tamara Berg", stage=Three} == DBLeafO 4 [CSStudent {sid = 1, sname = "Mike Brown", stage = One}, CSStudent {sid = 2, sname = "Georgia Jones", stage = Two}, CSStudent {sid = 3, sname = "Tamara Berg", stage = Three}] &&
  csInsert (csInsert (csInsert (csInsert csStud CSStudent {sid = 1, sname = "Mike Brown", stage = One}) CSStudent {sid=2, sname="Georgia Jones", stage=Two}) CSStudent {sid=3, sname="Tamara Berg", stage=Three}) CSStudent {sid=7, sname="Eric Han", stage=Four} ==
  DBLeafO 4 [CSStudent {sid = 1, sname = "Mike Brown", stage = One},
             CSStudent {sid = 2, sname = "Georgia Jones", stage = Two},
             CSStudent {sid = 3, sname = "Tamara Berg", stage = Three}]


{-
#Q2
-}
data Colour = Red | Orange | Yellow | Green | Blue | Indigo
    deriving (Eq, Show, Read)

{-
#Q2i
-}

newtype Code = Code (Colour, Colour, Colour, Colour)
  deriving (Eq, Show)

{-
#Q2ii
-}

code2List :: Code -> [Colour]
code2List (Code (a,b,c,d)) = [a,b,c,d]

list2Code :: [Colour] -> Code
list2Code [a,b,c,d] = Code (a,b,c,d)
list2Code _ = error "bad data passed to list2Code (wrong length)"

testCodeToFromList :: Bool
testCodeToFromList = let datum = [Red, Orange, Yellow, Green] in
  code2List(list2Code datum) == datum

{-
#Q2iii
-}

-- (right, wrong)
newtype Response = Response (Int, Int)
  deriving (Eq, Show)

{-
#Q2iv
-}

mkResponse :: Int -> Int -> Response
mkResponse right wrong = Response (right, wrong)

hits, near :: Response -> Int
hits (Response (right, _)) = right
near (Response (_, wrong)) = wrong

test_mkResponse :: Bool
test_mkResponse = let r = mkResponse 2 1 in (hits r, near r) == (2, 1)

{-
#Q2v
-}

oneRound :: Code -> Code -> Response
oneRound secret guess = Response (right, wrong)
  where
    right = length $ filter (\(x,y) -> x==y) $ zip secretL guessL
    wrong = (length $ filter (flip elem secretL) guessL) - right
    secretL = code2List secret
    guessL = code2List guess

test_oneRound :: Bool
test_oneRound = oneRound (list2Code [Red, Orange, Yellow, Green])
  (list2Code [Green, Blue, Yellow, Orange]) == mkResponse 1 2

{-
#Q2vi
-}

data Player = Encoder | Guesser deriving (Eq, Show)

data Winner = NoWinner | GameOver String

{-
#Q2vii
-}

winner :: Response -> Int -> Winner
winner (Response (r,_)) 0 = if r==4 then (GameOver "Guesser") else (GameOver "Encoder")
winner (Response (r,_)) _ = if r==4 then (GameOver "Guesser") else (NoWinner)

ppWinner :: Winner -> String
ppWinner (GameOver x) = x
ppWinner NoWinner = "No winner"

noWinner :: String
noWinner = "No winner"

test_winner :: Bool
test_winner = ppWinner (winner (mkResponse 2 1) 2) == noWinner
  && ppWinner (winner (mkResponse 2 1) 0) == "Encoder"

{-
#Q3
-}

newtype StockF item = StockF {getStockF :: item -> Int}
newtype StockL item = StockL {getStockL :: [(item, Int)]} deriving (Eq, Show)

data Item = Loaf_White_Small | Loaf_Brown_Large | Single_Apple | Raisins_1kg
    deriving (Eq, Show)

{-
#Q3i
-}

countF :: StockF item -> item -> Int
countF = getStockF

countL :: Eq item => StockL item -> item -> Int
countL xs x = maybe 0 id (lookup x (getStockL xs))

test_count :: Bool
test_count = let f Single_Apple = 30
                 f Raisins_1kg = 6
                 f _ = 0
  in countF (StockF f) Single_Apple == 30 &&
     countL (StockL [(Loaf_White_Small, 0), (Single_Apple, 30)]) Single_Apple == 30

{-
#Q3ii
-}

restockF :: StockF item -> StockF item -> StockF item
restockF old new = StockF f
  where
    f x = ((getStockF old) x) + ((getStockF new) x)

-- newtype StockL item = StockL {getStockL :: [(item, Int)]} deriving (Eq, Show)
restockL :: Eq item => StockL item -> StockL item -> StockL item
restockL oldStock newStock = StockL (map (\(i,n) -> (i, n+(countL newStock i))) (getStockL oldStock))

test_restock :: Bool
test_restock =
  let f Single_Apple = 30
      f Raisins_1kg = 6
      f _ = 0
      g Raisins_1kg = 3
      g Loaf_White_Small = 7
      g _ = 0
      r = restockF (StockF f) (StockF g)
  in countF r Single_Apple == 30 && countF r Raisins_1kg == 9 &&
  let r = restockL (StockL [(Single_Apple, 30),(Raisins_1kg, 6)]) (StockL [(Raisins_1kg, 3), (Loaf_White_Small, 7)])
  in countL r Single_Apple == 30 && countL r Raisins_1kg == 9

{-
#Q3iii
-}

fillOrderF :: StockF item -> StockF item -> StockF item
fillOrderF old new = StockF f
  where
    f x = max (((getStockF old) x) - ((getStockF new) x)) 0

  
fillOrderL :: Eq item => StockL item -> StockL item -> StockL item
fillOrderL old new = StockL (map (\(i,n) -> (i, max (n-(countL new i)) 0)) (getStockL old))


  
test_fillOrder :: Bool
test_fillOrder =
  let f Single_Apple = 30
      f Raisins_1kg = 6
      f _ = 0
      g Raisins_1kg = 9
      g Loaf_White_Small = 7
      g _ = 0
      r = fillOrderF (StockF f) (StockF g)
  in countF r Single_Apple == 30 && countF r Raisins_1kg == 0
  &&
  let r = fillOrderL (StockL [(Single_Apple, 30), (Raisins_1kg, 6)])
        (StockL [(Raisins_1kg, 9), (Loaf_White_Small, 7)])
  in countL r Single_Apple == 30 && countL r Raisins_1kg == 0

{-
#Q4
-}

infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show

{-

(.) :: (a -> b) -> (c -> a) -> (c -> b)

(f . g) = f (g x) -- (.).0

maybe :: a -> (b -> a) -> Maybe b -> a
maybe k f Nothing = k -- maybe.0
maybe k f (Just x) = f x -- maybe.1

FOR-ALL
g :: a -> b,
k :: a,
f :: c -> a

{g . maybe k f == maybe (g k) (g . f)}
-}
