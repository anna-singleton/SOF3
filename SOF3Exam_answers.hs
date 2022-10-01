module SOF3Exam2022 where
import Data.List
import Data.Maybe


{-
# SOF3 Main Assessment 2021-22

You may use any values defined in the standard `Prelude`, but you may
not import any other module.  You may use Neil Mitchell's
[Hoogλe](https://hoogle.haskell.org/) to discover useful functions in
the `Prelude`.

Where tests accompany the English description of the problem, it is
not necessarily enough for your implementation to pass the tests to be
correct.

## Question 1 [40 marks]

### Question 1(i) [1 mark]
Write a function `isGreater` that takes a number and a list of numbers and returns `True` if the number is greater than the sum of all the numbers in the list and `False` otherwise.

Your solution should satisfy:
-}

gTest :: Bool 
gTest = and (map isG [(37, [2, 4, 8], True), 
                      (14, [2, 4, 8], False),
                      (54, [1..10], False),
                      (56, [1..10], True )])
   where
   isG (i, j, k) = isGreater i j == k

isGreater :: (Num a, Ord a) => a -> [a] -> Bool 

isGreater x = (x >) . sum

{-
### Question 1(ii) [2 marks]
Write a function `numIn` that takes an item and a list as input, and returns the number of times the item occurs in the lists.

Your solution should satisfy:
-}
numTest :: Bool 
numTest = and (map isN [(37, [2, 4, 8, 37], 1), 
                      (4, [2, 4, 8, 4, 5, 4], 3),
                      (54, [1..10], 0)]) && 
                      isN ('o', "accommodation", 3)
   where
   isN (i, j, k) = numIn i j == k

numIn :: Eq a => a -> [a] -> Int


numIn x = length . findIndices (==x)

{-
### Question 1(iii) [2 marks]
Consider 
-}
data Pixel = B | W deriving Eq
{-
In a 2-dimensional binary image, each pixel can either be black (background) or white (foreground). Write a function `fPixels` that takes a binary image and returns the number of foreground pixels in the image.

Your solution should satisfy:
-}
pixelT :: Bool 
pixelT = and (map isPix [([[B,B,W],[B,B,B], [B,B,B]], 1), 
                      ([[B,W],[W,W], [W,B]], 4),
                      ([[B]], 0)])
   where
   isPix (i, k) = fPixels i == k

fPixels :: [[Pixel]] -> Int

fPixels = length . filter (==W) . concat


{-
### Question 1(iv) [5 marks]
Given a list of `Ord` type, write a function `repTimes` that returns a sorted list of pairs (aka an association list). The first element of each pair should be an element of the list, and the second element should be the number of occurrences of the element in the list.  Each element in the list should occur exactly once as the first element of a pair, and no other elements should appear as the first element. The list of pairs must have its elements in a sorted order (Hint: you may use `sort` from `Data.List`).

Your solution should satisfy:
-}

rTest :: Bool 
rTest = and (map isR [("software", [('a',1),('e',1),('f',1),('o',1),('r',1),('s',1),('t',1),('w',1)]),
                      ("mississippi",[('i',4),('m',1),('p',2),('s',4)]),
                      ("3365544", [('3',2),('4',2),('5',2),('6',1)])]) &&
                      isR ([2, 3, 1, 3, 4], [(1,1),(2,1),(3,2),(4,1)])
   where
   isR (i, j) = repTimes i == j


repTimes :: Ord a => [a] -> [(a, Int)]

repTimes = map (\x -> (head x, length x)) . group . sort


{-
### Question 1(v) [5 marks]

Write a function `revNum`, which takes a number and returns the number with its digits reversed. You may assume numbers are represented in Base 10 by `show`, and that strings of digits are converted to a number by `read`, again assuming Base 10. 

Your solution should satisfy:
-}

rnTest :: Bool
rnTest = (revNum 200         == 2) &&
         (revNum 73622385    == 58322637) &&
         (revNum 9034761030  == 301674309) 

revNum :: Integer -> Integer


revNum = read . reverse . show


{-
### Question 1(vi) [5 marks]
Write a function `sumMaybe` which takes two lists of type `Maybe a` and adds their corresponding values. The final list should have the same length as the shortest of the two lists. 

Your solution should satisfy:
-}
suTest :: Bool
suTest = and (map isMB [
        ([Just 2.5, Just 8.2, Nothing], [Just 5.2, Nothing, Just 6], [Just 7.7,Just 8.2,Just 6.0]),
        ([],[],[]),
        ([Just 33, Just 17, Nothing], [Just 12, Just 3, Just 71], [Just 45,Just 20,Just 71]),
        ([Just 33, Just 17, Nothing, Nothing, Just 20], [Just 12, Just 3, Just 71], [Just 45,Just 20,Just 71])])
   where
   isMB (i, j, k) = sumMaybe i j == k

{-
ambiguity here, There is no test case showing the correct behaviour for
Nothing + Nothing, so the test may fail. This code will evaluate Nothing +
Nothing to Just 0
-}
sumMaybe :: Num a => [Maybe a] -> [Maybe a] -> [Maybe a]
sumMaybe = zipWith f
  where
    f :: Num b => Maybe b -> Maybe b -> Maybe b
    f x y = Just $ (maybe 0 id x) + (maybe 0 id y)

{-
### Question 1(vii): [10 marks]  
A multi-way tree or a Rose Tree is a tree data structure with variable and unbounded number of branches per node. 
Given the type `RT a` for a non-empty Rose Tree, write a function `sameShape` which compares two non-empty Rose trees and returns `True` if they have the same structure and `False` otherwise. 

Your solution should satisfy:
-}

intRT, lgRT :: RT Int
chRT        :: RT Char
intRT = Node 2 [Node 3 [], Node 5 []]
chRT  = Node 'a' [Node 's' [], Node 'e' []]
lgRT = Node 2 [Node 3 [Node 45 [Node 4 [], Node 8 []]], Node 5 []]

rtTest :: Bool 
rtTest = and (map isRT [(intRT, lgRT, False),
                      (intRT, intRT, True),
                      (lgRT, lgRT, True)]) &&
                      (sameShape chRT intRT == True) &&
                      (sameShape chRT lgRT == False)
   where
   isRT (i, j, k) = sameShape i j == k

data RT a = Node a [RT a]
sameShape :: RT a -> RT b -> Bool

sameShape t1 t2 = (shape t1) == (shape t2)
  where
    shape :: RT a -> [Int]
    shape (Node _ ts) = length ts : concatMap shape ts

{-
do an pre-order traversal and compose this into a list of child amount
-}

{-
### Question 1(viii): [10 marks] 

The Caesar cipher is a very simple encryption method, and it is easily breakable. In this question we used an improved encryption method. Given a plain text message *T* of *n* characters using an alphabet *A* of *m* letters, and a list *S* of *n* positive integers from the set *{0..k-1}*, *T* is encrypted by shifting the *k-th* character *T[k]* by an amount *S[k]*, wrapping around at the end of the alphabet. An example is given a plain text `"BABY"` and integer list `[2, 1, 1, 3]`, the first character **'B'** is shifted by 2 giving the character **'D'**, the second character **'A'** is shifted by 1 giving **'B'** and so on, to return the encrypted message '"DBCB"'.

Write a function `encrypt` that takes a plain text as a string, a shift sequence as a list of integers, a list of Latin alphabets and returns a string containing the encrypted text. 

Your solution should satisfy:

-}

encTest :: Bool
encTest = and (map enT 
    [("BAC", [2, 1, 3], "ABCDE", "DBA"),
    ("BABY", [1, 1, 1, 1], alCaps, "CBCZ"),
    ("BABY", [2, 1, 1, 3], alCaps, "DBCB"),
    ("SOFTWARE", [2, 1, 1, 3, 4, 6, 7, 8], alCaps, "UPGWAGYM")])
     where
     enT (i,j,k,l) = encrypt i j k == l

alCaps :: [Char]
alCaps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


encrypt :: String -> [Int] -> String -> String

encrypt str shift alpha = zipWith f str shift
  where
    f chr mv = cycle alpha !! ((+mv) $ fromJust $ findIndex (==chr) alpha)

{-
## Question 2 [20 marks]

In this question you are asked to define types, as well as values.
Each type, *Name*, is given a default implementation as an alias for
the unit type: `type Name = ()`.  You may change the keyword `type` to
either `newtype` or `data` if appropriate, and you should replace the
unit type, `()`, by a suitable type expression.  You may derive any
type classes, such as `Show`, that will be useful.

Marks will be mostly given for how well the type protects the
programmer from mistakes, for its utility to the programmer, and also
partly for the run-time efficiency of the type.

The game of *mine sweeper* is a solo game played on a finite
rectangular grid that very loosely simulates detecting ordinance mines
prepartory to their removal.  Each square is either "safe" or
"unsafe".

A player visits squares in any order they choose.  When a player
visits a square one of two things will happen:

1. if the square is safe, the number of unsafe adjacent squares is
   revealed; and
2. if the square is unsafe, the game ends.

The player's aim is to reveal the positions of all the safe squares
without visiting an unsafe square.

Locations in the grid are modelled as values of type `Coord`:
-}
type Coord = (Int, Int)
{-
Legal grid locations start from 1, not 0, in both directions.

### Question 2(i) [12 marks]

Give types to represent the safeness of squares, `Unsafe`, and grids
themselves, `Grid`, which record the dimensions of the grid, and the
safeness or otherwise of each square in the grid.
-}
type Unsafe = Bool -- REPLACE THIS DEFINITION
type Grid   = (Int, Int, Coord->Unsafe) -- REPLACE THIS DEFINITION

{-
### Question 2(ii) [4 marks]

Give functions that convert a triple consisting of a pair of integers,
representing the dimensions of the grid, and list of coordinate pairs,
representing the location of unsafe squares, to a `Grid`, `toGrid`,
and from a `Grid` to a triple, `fromGrid`.  For example,
```haskell
toGrid (7, 10, [(3, 8), (4, 9)])
```
is a 7-by-10 grid with two mines, at locations `(3, 8)` and `(4, 9)`.

You may assume that only values that construct legal grids are passed
into `toGrid`.
-}

toGrid :: (Int, Int, [Coord]) -> Grid
fromGrid :: Grid -> (Int, Int, [Coord])
toGrid (x, y, mines) = (x, y, f)
  where
    f :: Coord -> Unsafe
    f = flip elem mines
fromGrid (xlim, ylim, f) = (xlim, ylim, [(x,y) | x <- [1..xlim], y <- [1..ylim], f (x,y)])

{-
Your functions should satisfy:
-}
testToFromGrid :: Bool
testToFromGrid = (9, 5, []) == fromGrid (toGrid (9, 5, [])) 

{-
### Question 2(iii) [4 marks]

Give the function that reports the result of visiting a square.
The inputs to the function are:
1. the grid
2. the x-coordinate (starting from 1)
3. the y-coordinate (starting from 1)

You may assume that only legal coordinates (that is, coordinates
representing a square in the grid) are given.
-}
visit :: Grid -> Coord -> Maybe Int
visit (_,_,f) c@(x,y) | f c = Nothing
                      | otherwise = Just . length . filter f $ neighbours c
  where
    neighbours (x,y) = [(a,b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)]]

{-
Your function should satisfy:
-}
testVisit :: Bool
testVisit = map (visit (toGrid (9, 5, [(3, 4), (1, 4)]))) [(3, 4),  (2, 4)]
            ==                                            [Nothing, Just 2]
            
{-
## Question 3 [15 marks]

Consider a club that wishes to commision software to hold membership
details.  They can hold data in at least two different ways:
* a list of pairs of names/details.

-}
type DetailsLP n d = [(n, d)]
{-
* a pair of lists, of equal length, the first holding names, and the
  second holding the details of the name at the same index.

-}
type DetailsPL n d = ([n], [d])
{-
For each of the tasks 3(i), 3(ii) and 3(iii) below, give **two*
solutions, one using each type.

### Question 3(i) [2 marks]

Give the value representing the initial (that is, empty) state of the
database.
-}
emptyLP :: DetailsLP n d
emptyPL :: DetailsPL n d
emptyLP = []
emptyPL = ([], [])

{-
### Question 3(ii) [4 marks]

Give the functions that return the details of a club member.  You may
assume that the function is always called with a name known to be that
of a club member.
-}
detailsLP :: Eq n => n -> DetailsLP n d -> d
detailsPL :: Eq n => n -> DetailsPL n d -> d
detailsLP x ds = fromJust $ lookup x ds
detailsPL x ds = detailsLP x $ uncurry zip ds
{-
### Question 3(iii) [6 marks]

Assuming that the lists are sorted by name, give the function that
inserts/modifies a name/details pair into a database in each case.  If
the name is new, then it and the details are inserted in the correct
place.  If the name is already present, then the details are replaced
with the input details.
-}
insertLP :: Ord n => n -> d -> DetailsLP n d -> DetailsLP n d
insertPL :: Ord n => n -> d -> DetailsPL n d -> DetailsPL n d
insertLP name det (a@(n,d):ds) | name==n = (n,det) : ds
                               | name<n = (name,det):a:ds
                               | otherwise = a : (insertLP name det ds)
insertLP name det _ = [(name,det)]
insertPL name det xs = unzip $ insertLP name det $ uncurry zip xs

{-
### Question 3(iv) [3 marks]

Give the functions that convert between the two representations,
`DetailsLP` and `DetailsPL`.

-}
detailsLP2PL :: DetailsLP n d -> DetailsPL n d
detailsPL2LP :: DetailsPL n d -> DetailsLP n d
detailsLP2PL = unzip
detailsPL2LP = uncurry zip

{-
Your functions should satisfy:
-}
testDetailsLP :: Bool
testDetailsLP = detailsLP "k" (insertLP "j" 0 (insertLP "k" 1 emptyLP)) == 1
testDetailsPL = detailsPL "j" (insertPL "j" 0 (insertPL "k" 1 emptyPL)) == 0

{-
## Question 4 [10 marks]

Recall the definition of the `ProofLayout` type constructor:
-}
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
{-
Consider the definitions:
```haskell
length :: [a] -> Int
length []     = 0             -- length.0
length (_:xs) = 1 + length xs -- length.1

map :: (a- -> b) -> [a] -> [b]
map _ []     = []             -- map.0
map f (x:xs) = f x : map f xs -- map.1

(.) :: (a -> b) -> (c -> a) -> c -> b
(f . g) = f (g x) -- (.).0 
```
Give a value `proof4` that proves the theorem:
```haskell
forall f :: a -> b {length = length . (map f)}
```
**Hint** Use structural induction over lists.
-}

{-
i am not redoing this i cannot be fucked
-}
proof4 :: (a -> b) -> [a] -> ProofLayout Int
proof4 = undefined

{-
## Question 5 [5 marks]

Give a function that takes as input a `FilePath` (that is, a `String`
representing the name of a file), and prints the length of each line
in the order the lines occur in the file.

You do **not** need to deal with the case of the file being missing.
-}
lineLengths :: FilePath -> IO ()
lineLengths fp = do
  inp <- readFile fp
  strs <- lines <$> pure inp
  nums <- map (show . length) <$> pure strs
  result <- unlines <$> pure nums
  putStr result

{-
If the file contents are:

> hello
>
> worlds
>
> !

The output should be:

> 5
> 0
> 6
> 0
> 1

-}

