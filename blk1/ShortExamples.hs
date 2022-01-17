-- JLJ 14 Jan 2021

module ShortExamples where

{-
The first program seen in most languages is "Hello World".
Here it is in Haskell, given the name `hw`:
-}

hw :: String -- `hw` is the name of an expression that evaluates to a string
hw = "Hello World"

{-
When `hw` is evaluated in a Haskell REPL the output includes the
opening and closing quote marks (`"`).  A version that does not print
the quote marks uses the function `putStrLn` ("put a string to the
standard output, and then a newline"):
-}

hwp :: IO () -- hwp is an input/output action that carries the `unit` (null) value
hwp = putStrLn hw -- reuse of `hw`

{-
There is an important technical difference between `hw` and `hwp`,
exposed by their types, which we will deal with in Block 4.

Now consider a numeric example.  Define a function that:
* if given an even `Integer`, outputs half that number
* otherwise, output the number one larger

We will call this f0:
-}

f0 :: Integer -> Integer
f0 = \n -> if even n then div n 2 else n + 1

{-
Here are other, equivalent, ways we could write `f0`
-}

f1, f2, f3 :: Integer -> Integer

f1 = \n -> (if even n then flip div 2 else succ) n

f2 n | even n    = div n 2
     | otherwise = n + 1

f3 n = go n
  where
    go | even n    = flip div 2
       | otherwise = succ

{-
`f3` is probably the version most experienced Haskell programmers would use.
* It uses patterns and guards, rather than explict λ epressions and `if`.
* It separates out the differences between the two branches
  from the commonality within them.

We can start with a number and apply one of the variants every
possible number of times.
-}

fAll :: Integer -> [Integer]
fAll = iterate f3

{-
The infinite computation is not a problem if we only look at a finite
part of it.

For example, we can look at  the first 20 elements:
-}

twentyF :: Integer -> [Integer]
twentyF = take 20 . fAll

{-
Or until we get a value in the range [0 .. 2]:
-}

inRangeF :: Integer -> [Integer]
inRangeF = takeWhile (not . flip elem [0 .. 2]) . fAll

{-
We can look for the first time a value is repeated, and so a cycle begins:
-}

findStableF :: Integer -> Integer
findStableF = go [] .  fAll
  where
    go seen (x:xs) | x `elem` seen = x
                   | otherwise     = go (x:seen) xs

{-
These functions have a common pattern that we can abstract:
-}

searchF :: ([Integer] -> c) -> Integer -> c
searchF g = g . fAll

twentyF' :: Integer -> [Integer]
twentyF' = searchF (take 20)

inRangeF' :: Integer -> [Integer]
inRangeF' = searchF (takeWhile (not . flip elem [0 .. 2]))

findStableF' :: Integer -> Integer
findStableF' = searchF (go [])
  where
    go seen (x:xs) | x `elem` seen = x
                   | otherwise     = go (x:seen) xs
