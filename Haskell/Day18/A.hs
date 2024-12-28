import Data.Bifunctor (first, second)
import qualified Data.Map as M

main1, main2 :: IO ()
main1 = readFile "./input.txt" >>= print . solve 1024  . toInput . lines
main2 = readFile "./input.txt" >>= print . solve2 . toInput . lines

toInput :: [String] -> [(Int, Int)]
toInput = map f
 where
  f s = let (l, r) = span (/=',') s in (read l, read (tail r))

type Grid = M.Map (Int, Int) Char

initialGrid :: Int -> Grid
initialGrid dim = M.fromList $ map (,'.') $ (,) <$> [0..dim] <*> [0..dim]

addBits :: Grid -> [(Int, Int)] -> Grid
addBits = foldr (`M.insert` '#')

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

solve :: Int -> [(Int, Int)] -> Maybe Int
solve bytes  bs = 
  let m = bfs [((0,0), 0)] (addBits (initialGrid 70) (take bytes bs)) M.empty
  in m M.!? (70, 70)

solve2 :: [(Int, Int)] -> (Int, Int)
solve2 bs = bs !! search bs 0 (length bs)

search :: [(Int, Int)] -> Int -> Int -> Int
search bs l r | l <= r = 
  let mid = (l + r) - l 
  in case solve mid bs of
          Nothing -> search bs l (mid - 1)
          Just _  -> search bs (mid + 1) r
              | otherwise = l - 1