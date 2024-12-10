import Data.Maybe
import Data.List (partition)
import Data.Function (on)

main :: IO ()
main = readFile "./input.txt"  >>= print . result . fill . disk . map (read . return)

result :: [(Int, Maybe Int)] -> Int
result = sum . zipWith (*) [0..] . concatMap (\(x, y) -> replicate x (fromMaybe 0 y))


-- | Generates pairs of segment length and content from given sequence
disk :: [Int] -> [(Int, Maybe Int)]
disk = f 0 . zip [0..]
 where 
  f :: Int -> [(Int, Int)] -> [(Int, Maybe Int)]
  f n [] = []
  f n ((index, v) : xs) | even index = (v , Just n) : f (n + 1) xs
                        | odd  index = (v, Nothing) : f n xs

-- | Moves complete segments from the end to the first open space. 
--   Only tries to move each segment once, starting with the last.
fill :: [(Int, Maybe Int)] -> [(Int, Maybe Int)]
fill mis = let (filled, free) = partition (isJust . snd) mis
            in f mis (reverse filled)
 where
  f :: [(Int, Maybe Int)] -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
  f xs []       = xs
  f xs (g : gs) = let xs' = tryFit g xs in f xs' gs

  tryFit :: (Int, Maybe Int) -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
  tryFit (l, v) [] = []
  tryFit p (x : xs) 
    | p == x            = x : xs
    | isNothing (snd x) = case (compare `on` fst) p x of
                            GT -> x : tryFit p xs
                            EQ -> let xs' = nullyfy (fromJust $ snd p) xs in p : xs'
                            LT -> let xs' = nullyfy (fromJust $ snd p) xs
                                      spaceLeft = (((-) `on` fst) x p, Nothing)  
                                  in p : spaceLeft : xs'
    | otherwise = x : tryFit p xs

-- | Erases a segment with name @v from the given list
nullyfy :: Eq v => v -> [(k, Maybe v)] -> [(k, Maybe v)]
nullyfy _ [] = []
nullyfy v (p@(k', Just v') : xs) 
  | v == v'   = (k', Nothing) : xs
  | otherwise = p : nullyfy v xs
nullyfy v (p@(k', Nothing) : xs) = p : nullyfy v xs