import Data.Maybe
import Data.List (partition)
import Data.Function (on)

main :: IO ()
main = readFile "./input.txt"  >>= print . result . fill . disk . map (read . return)

result :: [Maybe Int] -> Int
result = sum . zipWith (*) [0..] . catMaybes

-- | Unfolding the input. Could be improved by using tuples that encode segments
disk :: [Int] -> [Maybe Int]
disk = concat . f 0 . zip [0..]
 where
  f :: Int -> [(Int, Int)] -> [[Maybe Int]]
  f n []                = []
  f n ((index, v) : xs) | even index = replicate v (Just n) : f (n + 1) xs
                        | odd  index = replicate v Nothing  : f n xs

-- | Moves parts of segments to open spaces starting at the last position
fill :: [Maybe Int] -> [Maybe Int]
fill mis = let (filled, free) = partition isJust mis 
               goal           = length filled
           in f goal mis (reverse filled)
 where
  f :: Int -> [Maybe Int] -> [Maybe Int] -> [Maybe Int]
  f 0 _        _      = []
  f n (m : ms) (x : xs) | isNothing m = x : f (n - 1) ms xs
                        | otherwise   = m : f (n - 1) ms (x:xs)  