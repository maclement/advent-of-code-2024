import qualified Data.Map as M

import Data.Bifunctor (first, second)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (group, sort)

type Grid = M.Map (Int, Int) Char

toGrid :: [String] -> Grid
toGrid input = M.fromList $
   [ ((y, x), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]

directions :: [(Int, Int) -> (Int, Int)]
directions = [first succ, first pred, second succ, second pred]

bfs :: [((Int, Int), Int)] -> Grid -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
bfs [] _ visited = visited
bfs (hq : rq) g visited =
 let unVisited = filter (`M.notMember` visited) (($ fst hq) <$> directions)
     reachable = filter (\x -> maybe False (/='#') (g M.!? x)) unVisited
     visisted' = foldr (\a b -> M.insert a (succ $ snd hq) b) visited reachable
     queue     = rq ++ zip reachable (repeat (succ $ snd hq))
 in bfs queue g visisted'

solve :: Int -> Grid -> Int
solve i g = 
  let (startX, startY) = fst $ head $ M.toList $ M.filter (=='S') g
      (goalX, goalY)   = fst $ head $ M.toList $ M.filter (=='E') g
      mapFromStart     = bfs [((startX, startY), 0)] g (M.singleton (startX, startY) 0)
      mapFromGoal      = bfs [((goalX, goalY)  , 0)] g (M.singleton (goalX , goalY)  0)
      originalDist     = mapFromStart M.! (goalX, goalY)
      cheatingDists    = [ fromJust toStart + dif + fromJust toEnd 
                         | let idcs = map fst $ M.toList $ M.filter (/='#') g
                         , (x1, y1) <- idcs
                         , (x2, y2) <- range (x1, y1) g
                         , let dif = abs (x1 - x2) + abs (y1 - y2) 
                         , dif <= i
                         , let toStart = mapFromStart M.!? (x1, y1)
                         , let toEnd = mapFromGoal M.!? (x2, y2)
                         , isJust toStart && isJust toEnd
                         ]
  in sum . map length . group . sort . filter (>=100) . map (originalDist-) $ cheatingDists

range :: (Int, Int) -> M.Map (Int, Int) Char -> [(Int, Int)]
range (x, y) m = [ (x', y') | x' <- [(x - 20) .. (x + 20)], y' <- [(y - 20) .. (y + 20)], maybe False (/= '#') (m M.!? (x', y')) ]

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve 2  . toGrid  . lines
main2 = readFile "./input.txt"  >>= print . solve 20 . toGrid  . lines