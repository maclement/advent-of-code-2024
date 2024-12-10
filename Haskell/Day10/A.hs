import Data.Bifunctor (bimap, first, second)
import Data.Maybe (isNothing, isJust)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve1 . toGrid . lines
main2 = readFile "./input.txt"  >>= print . solve2. toGrid . lines

test = readFile "./input.txt"  >>= print . toGrid . lines

type Coordinate = (Int, Int)
type Grid = M.Map Coordinate Int

toGrid :: [String] -> Grid
toGrid input = M.fromList
  [ ((y, x), read $ return char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]

solve1 :: Grid -> Int
solve1 g = sum [ length $ S.toList $ findPaths g  startPoint | startPoint <- M.keys $ M.filter (== 0) g]

findPaths :: Grid -> Coordinate -> Set Coordinate
findPaths g c | g M.!? c == Just 9 = S.singleton c
              | otherwise          = foldr S.union S.empty $ [ findPaths g n | n <- neighborhood c, isJust $ g M.!? n, (g M.! c) + 1 == g M.! n]


neighborhood :: Coordinate -> [Coordinate]
neighborhood p = [ first succ p, second succ p, first pred p, second pred p ]

solve2 :: Grid -> Int
solve2 g = sum [ reachesTop g startPoint  | startPoint <- M.keys $ M.filter (== 0) g]

reachesTop :: Grid -> Coordinate -> Int
reachesTop g c | g M.!? c == Just 9 = 1
               | otherwise          = sum $ [reachesTop g n | n <- neighborhood c, isJust $ g M.!? n, (g M.! c) + 1 == g M.! n]