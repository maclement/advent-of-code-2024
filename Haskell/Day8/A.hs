import qualified Data.Map as M
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (nub)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve1 . toGrid . lines
main2 = readFile "./input.txt"  >>= print . solve2. toGrid . lines

type Coordinate = (Int, Int)
type Grid = M.Map Coordinate Char

toGrid :: [String] -> Grid
toGrid input = M.fromList
  [ ((y, x), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]


instance Num (Int, Int) where
    (a, b) - (c, d) = (a - c, b - d)  
    (a, b) + (c, d) = (a + c, b + d)
    abs = bimap abs abs

solve1 :: Grid -> Int
solve1 g = length $ nub
  [ x
  | a <- M.keys (M.filter (/= '.') g)
  , let ca = g M.! a
  , b <- M.keys (M.filter (== ca) g)
  , a /= b
  , x <- [b + (b - a), a + (a - b)]
  , x `elem` M.keys g
  ]

solve2 :: Grid -> Int
solve2 g = length $ nub $ concat
  [ iterateInRange (M.keys g) b (b - a) ++ iterateInRange (M.keys g) a (a - b)
  | a <- M.keys (M.filter (/= '.') g)
  , let ca = g M.! a
  , b <- M.keys (M.filter (== ca) g)
  , a /= b
  ]

iterateInRange :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
iterateInRange range start move = takeWhile (`elem` range) $ iterate (+move) start