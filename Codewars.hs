module Codewars where
import Data.Char
import Data.List

squareDigit :: Int -> Int
squareDigit x | x < 0 = read $ "-" ++ (show (getDigits (negate x)))
              | otherwise = getDigits x
  where
    getDigits :: Int -> Int
    getDigits y = read $ concat $ map show $ map ((^2) . digitToInt) $ show y

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers x = sum $ take n $ drop (((n)*(n-1))`div`2) odds
  where odds = [y | y <- [1..], y `mod` 2 == 1]
        n :: Int
        n = fromIntegral x


-- formatDuration :: (Integral i) => i -> [String]
-- formatDuration s = do
--     years <- s `div` year
--     x <- s `mod` year
--     days <- x `div` day
--     x <- x `mod` day
--     hours <- x `div` hour
--     x <- x `mod` hour
--     minutes <- x `div` minute
--     x <- x `mod` minute
--     [genStr years "year", genStr days "day", genStr hours "hour", genStr minutes "minute", genStr x "second"]
--     -- l <- filter (/= "") l
--     -- case (length l) of
--     --   0 -> ""
--     --   1 -> head l
--     --   otherwise -> (intercalate ", " (init l)) ++ " and " ++ (last l)
    
--   where 
--   genStr :: Int -> String -> String
--   genStr x s = case x of
--                  0 -> ""
--                  1 -> "1 " ++ s
--                  otherwise -> (show x) ++ " " ++ s
--   year = day*365
--   day = hour*24
--   hour = minute*60
--   minute = 60


chain :: a -> [a -> a] -> a
chain x fs = foldl apply x fs
  where
    apply :: a -> (a -> a) -> a
    apply y f = f y


expd :: Int -> String
expd = intercalate " + " . map show . unfoldr next
  where
    next :: Int -> Maybe (Int, Int)
    next x = if (x <= 0) then Nothing else
      if x < 10 then Just (x, -1) else Just $ ((x `div` (getOrder x))*(getOrder x), x `mod` (getOrder x))
    getOrder :: Int -> Int
    getOrder x = last $ takeWhile (<=x) $ iterate (*10) 1


sumMaybeIntStructure :: [Maybe Int] -> Maybe Int
sumMaybeIntStructure xs = undefined
