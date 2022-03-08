module Block4Q where
import Ncrosses
import System.IO
{-
# SOF3: Block 4 problems

# Problems

## Study

You should read [Hutton][1] Chapters 10–⁠12, 16; ensure that you do all
the exercises.

You should study the lecture on "Effects".

## Q1: Proof
Recall the `ProofLayout` type, and associated functions:
-}
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
instance Foldable ProofLayout where
  foldr f z = ffz
    where
      ffz QED        = z
      ffz (p :=: pl) = f p (ffz pl)
testPL :: Eq a => ProofLayout a -> Bool
testPL            QED            = True
testPL            (p :=: pl)     = all (==p) pl
{-
Also recall the `Prelude` functions `id`, `not` and `flip, repeated
here with labels on each line:

```haskell
id :: a -> a
id    x  = x -- id.0

not :: Bool -> Bool
not    True  = False -- not.0
not    False = True  -- not.1

flip :: (a -> b -c) -> b -> a -> c
flip    f              y    x  = f x y -- flip.0
```
Now consider the following definition:
-}
niff :: Bool -> Bool -> Bool
niff    True  = not          -- niff.0
niff    False = id           -- niff.1
{-
`niff` is commutative.  We can express this in Haskell as:
```haskell
flip niff = niff
```
Prove this theorem.
**Hint** Prove this as four cases, one for each pair of `Bool` values.
-}

iff :: Bool -> Bool -> Bool
iff = (==)

niffCommutative :: Bool -> Bool -> ProofLayout Bool
niffCommutative True  True  = niff True True
                              :=: -- niff.0
                              not True
                              :=: -- not.0
                              False
                              :=: -- not.0
                              not True
                              :=: -- niff.0
                              niff True True
                              :=: -- flip.0
                              flip niff True True
                              :=:
                              QED

niffCommutative True  False = flip niff True False
                              :=: -- flip.0
                              niff False True
                              :=: -- niff.1
                              id True
                              :=: -- id.0
                              True
                              :=: -- not.0
                              not False
                              :=: -- niff.0
                              niff True False
                              :=:
                              QED

niffCommutative False True  = flip niff False True
                              :=: -- flip.0
                              niff True False
                              :=: -- niff.0
                              not False
                              :=: -- not.0
                              True
                              :=: -- id.0
                              id False 
                              :=: -- niff.1
                              niff False True
                              :=:
                              QED

niffCommutative False False = flip niff False False
                              :=: -- flip.0
                              niff False False
                              :=: -- niff.1
                              id False
                              :=: -- id.0
                              False
                              :=: -- id.0
                              id False
                              :=:
                              niff False False
                              :=:
                              QED


testNiffProof :: Bool
testNiffProof = foldr (&&) True [testPL (niffCommutative x y) | x <- [True, False] , y <- [True, False]]

{-
## Q2: Interactive tic-tac-toe
In Chapter 11 of [Hutton][1], an interactive program that plays the game of tic-tac-toe (noughts-and-crosses) is introduced. We will focus on the version that allows two human players to compete against each other. The function `tictactoe` can be used to play the game with another human player. However, the game assumes player `O` goes first all the time. 

Write a function `fPlayer :: IO ()`, which offers any of the two players `O` or `X` the chance to go first. The function should first display a message like "Who wants to play first: " and expect either `O` or `X`, the response should be used to start the game. A message should be displayed for any other INVALID input and the players offered another chance to select a valid player to go first.
-}
fPlayer :: IO ()  
fPlayer = do
  putStrLn "Who is playing first? (X/O)... "
  c <- getLine
  case c of
    "X" -> tictactoe' X
    "O" -> tictactoe' O
    _ -> putStrLn "Invalid Input, must select either X or O"
    

{-
## Q3: Interactive tic-tac-toe 
In the tic-tac-toe game, the winner is printed on the screen. To keep track of the last winner, you will need a file
`champion.txt`. Extend the `Ncrosses.hs` file with a function `winner` which updates the `champion.txt` file.
If the game is played for the very first time and the `champion.txt` file does not exist, create it, and only update it
whenever there is a winner. You don't need to update the file when the game is a draw and only the last winner is recorded
in the `champion.txt` file. You may also update any of the functions in `Ncrosses.hs` to apply the `winner` function. 
-}


--winner :: String -> IO ()
--winner = writeFile "champion.txt"

-- see ln 137/8 of Ncrosses.hs

{-
## Q4: Interactive tic-tac-toe
Assume the file `champion.txt` exist with the player who last won the game. Modify the `fPlayer` function as `fPlayer'` which displays a string informing you of the last winner before you are given the option to choose the player to go first.
-}

fPlayer' :: IO ()  
fPlayer' = do
  prevWinner <- readFile "champion.txt"
  putStr "previous winner: "
  putStrLn prevWinner
  fPlayer

{-
## References

[1]: Graham Hutton, _Programming in Haskell_, Cambridge, 2nd edition,
     2016.  JMB shelfmark: SK 59 HAS/H; [Electronic
     version](https://doi-org.libproxy.york.ac.uk/10.1017/CBO9781316784099)

[2]: <https://hoogle.haskell.org/> Neil Mitchell, _Hoogλe_

-}
