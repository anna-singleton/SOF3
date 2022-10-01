module MonadPractice where
import Data.Maybe

mPlus3 :: Int -> Maybe Int
mPlus3 x = Just (x+3)

tAdd :: (Int, Int) -> Maybe Int
tAdd (x,y) = Just (x+y)

mAdd :: Maybe Int -> Maybe Int -> Maybe Int
mAdd xm ym = do
  x <- xm
  y <- ym
  pure (x + y)

sumMaybe :: [Maybe Int] -> Maybe Int
sumMaybe = foldr f (Just 0) 
  where
    f xm ym = do
      x <- xm
      y <- ym
      pure (x + y)


sumMaybe' :: [Maybe Int] -> Maybe Int
sumMaybe' xs | Nothing `elem` xs = Nothing
             | otherwise = pure $ sum $ map fromJust xs
