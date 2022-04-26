module Mock2 where

import Data.Maybe

data Player = Red | Green deriving (Eq, Show)

data Position = Start --off board
  | Sq_1 | Sq_2 | Sq_3 | Sq_4 -- private
  | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12 -- shared
  | Sq13 | Sq14 -- private
  | Home -- off board
  deriving
    (Eq, Ord, Enum, Bounded, Show)

piecesPerPlayer :: Int
piecesPerPlayer = 7

type Placement = (Position, Player) -> Int

data GameState = GameState Placement Player

opponent :: Player -> Player
opponent Red = Green
opponent Green = Red

test_opponent :: Bool
test_opponent = opponent Red == Green

isValidRoll :: Int -> Bool
isValidRoll x = x >= 0 && x <= 4

test_isValidRoll :: Bool
test_isValidRoll = isValidRoll 2 && not (isValidRoll 9)

plus :: Position -> Int -> Position
plus Home _ = Home
plus pos 0 = pos
plus pos x = plus (succ pos) (x-1)

test_plus :: Bool
test_plus = Start `plus` 0 == Start &&
            Start `plus` 3 == Sq_3 &&
            Sq_3 `plus` 2 == Sq_5 &&
            Sq14 `plus` 4 == Home


toList :: Placement -> [((Position, Player), Int)]
toList p = [((x, y), z) | x <- squares, y <- [Red, Green], z <- [p (x,y)]]
  where squares = enumFrom Start

fromList :: [((Position, Player), Int)] -> Placement
fromList xs = getVal
  where getVal x | lookup x xs == Nothing = 0
                 | otherwise = fromJust (lookup x xs)

test_ToFromList :: Bool
test_ToFromList = ((Sq_3, Red), 9) `elem` toList(fromList[((Sq_3, Red), 9)])

instance Eq GameState where
  GameState p x == GameState q y = (toList p, x) == (toList q, y)

validPlacement :: Placement -> Bool
validPlacement = validList . toList

validList :: [((Position, Player), Int)] -> Bool
validList xs = getPlayerCounters Red == 7 && getPlayerCounters Green == 7 && (all checkvalidTile (map (fst . fst) xs))
  where getPlayerCounters p = foldr ((+) . snd) 0 $ filter ((==) p . snd . fst) xs
        checkvalidTile sq | sq `elem` [Start, Home] = (getPCountersTile Red sq) <= piecesPerPlayer &&
                                                      (getPCountersTile Green sq) <= piecesPerPlayer
                          | otherwise = (getPCountersTile Red sq) <= 1 &&
                                        (getPCountersTile Green sq) <= 1
        getPCountersTile play tile = snd . head $ filter ((==) tile . fst . fst) $ filter ((==) play . snd . fst) xs 

test_validPlacement :: Bool
test_validPlacement =
  not (validPlacement (fromList [((Start, Red), 9), ((Start, Green), 7)]))
  && not (validPlacement (fromList [((Start, Red), 6), ((Start, Green), 7)]))

initGS :: GameState
initGS = GameState (fromList [((Start, Green), 7), ((Start, Red), 7)]) Red

test_initGS_placement :: Bool
test_initGS_placement = validPlacement plac
  && plac (Start, Red) == 7
  && plac (Sq10, Green) == 0
  && plac (Home, Red) == 0
  && rd == Red
  where
    GameState plac rd = initGS

possibleMoves :: GameState -> Int -> [Position]
possibleMoves = undefined

test_possibleMoves :: Bool
test_possibleMoves = possibleMoves initGS 0 == []
                     && possibleMoves initGS 3 == [Start]

move :: GameState -> (Int, Position) -> GameState
move = undefined

test_move :: Bool
test_move = plac1 (Start, Red) == pred piecesPerPlayer
            && plac1 (Sq_1, Red) == 1
            && plac1 (Sq_2, Red) == 0
            && plac1 (Start, Green) == piecesPerPlayer
            && plac1 (Sq_1, Green) == 0
            && plr1 == Green
            && plac2 (Start, Red) == pred piecesPerPlayer
            && plac2 (Sq_1, Red) == 1
            && plac2 (Sq_2, Red) == 0
            && plac2 (Start, Green) == pred piecesPerPlayer
            && plac2 (Sq_1, Green) == 0
            && plac2 (Sq_2, Green) == 1
            && plr2 == Red
            && plac2' (Start, Green) == piecesPerPlayer
            && plac2' (Sq_1, Red) == 1
            && plac2' (Sq_2, Red) == 0
            && plac2' (Sq_2, Green) == 0
            && plr2' == Green
  where
    gs1 = move initGS (1, Start)
    GameState plac1 plr1 = gs1
    gs2 = move gs1 (2, Start)
    GameState plac2 plr2 = gs2
    gs2' = move gs1 (5, Start)
    GameState plac2' plr2' = gs2'


gameOver :: GameState -> Maybe Player
gameOver = undefined

test_gameOver :: Bool
test_gameOver = gameOver initGS == Nothing

playSequence :: [(Int, Position)] -> GameState
playSequence = undefined

test_playSequence_gameOver :: Bool
test_playSequence_gameOver =
  gameOver (playSequence []) == Nothing &&
  gameOver (playSequence [(4, Start), (4, Sq_4)]) == Nothing &&
  gameOver (playSequence (take 40 seq1)) == Nothing &&
  gameOver (playSequence (take 41 seq1)) == Just Red &&
  gameOver (playSequence (take 42 seq2)) == Just Green
  where
    seq1 = cycle [(4, Start), (4, Sq_4), (4, Sq_8), (0, Start), (4, Sq12), (0, Start)]
    seq2 = (0, Start) : seq1
