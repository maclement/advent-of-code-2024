import qualified Data.Map as M
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Maybe (listToMaybe, isJust, fromJust)

type Coordinate = (Int, Int)
type Grid = M.Map Coordinate Char

toGrid :: [String] -> Grid
toGrid input = M.fromList
  [ ((x, y), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print .  solve1 . toGrid . lines
main2 = readFile "./input.txt"  >>= print .  solve2 . toGrid . lines

-- | Computes the indices of the neighboorhood including diagonals
stepFunctions :: [Coordinate -> Coordinate]
stepFunctions = [ bimap f g | f <- [succ, id, pred], g <- [succ, id, pred]]

-- | Naiivly iterates a step function within the bounds of the grid
safeIterate :: Coordinate -> Grid -> (Coordinate -> Coordinate) -> String
safeIterate c g f = drop 1 $ map (g M.!) $ takeWhile (`elem` M.keys g) (iterate f c)

solve1 :: Grid -> Int
solve1 g = length
  [ ()
  | c <- M.keys (M.filter (== 'X') g)
  , f <- stepFunctions
  , let text = take 3 $ safeIterate c g f
  , text == "MAS"
  ]

solve2 :: Grid -> Int
solve2 g = length
  [(topleft, topright, botleft, botright) |
     let directions
           = [bimap succ succ, bimap pred pred, bimap pred succ,
              bimap succ pred],
     c <- M.keys (M.filter (== 'A') g),
     let results = map (listToMaybe . safeIterate c g) directions,
     all isJust results,
     let [topleft, botright, topright, botleft] = map fromJust results,
     let diag1
           = ('S' == topright && 'M' == botleft)
               || ('S' == botleft && 'M' == topright),
     let diag2
           = ('S' == topleft && 'M' == botright)
               || ('S' == botright && 'M' == topleft),
     diag1 && diag2]