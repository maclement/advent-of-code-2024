import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor ( Bifunctor(second, first) )
import Data.Maybe (isJust, fromJust, isNothing)
import GHC.IO.Unsafe (unsafePerformIO)
import Data.List (nub)

type Grid a = M.Map (Int, Int) a
type Group = S.Set (Int, Int)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve1 . toGrid . lines
main2 = readFile "./input.txt"  >>= print . solve2 . toGrid . lines

toGrid :: [String] -> Grid Char
toGrid input = M.fromList
  [ ((y, x), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]

-- | direct neighborhood, the fancy way
directions :: [(Int, Int) -> (Int, Int)]
directions = [first, second] <*> [succ, pred]

separateGroups :: Eq a => Grid a -> [Group]
separateGroups g = go g S.empty (M.keys g)
 where
  go :: Eq a => Grid a -> S.Set (Int, Int) -> [(Int, Int)] -> [Group]
  go g visited []                                 = []
  go g visited (p : ps) | p `S.notMember` visited = let group = findGroupFromPosition' g p
                                                    in group : go g (visited `S.union` group) ps
                        | otherwise               = go g visited ps

findGroupFromPosition' :: Eq a => Grid a -> (Int, Int) -> Group
findGroupFromPosition' g i = go S.empty g [i]
 where
  go :: Eq a => Group -> Grid a -> [(Int, Int)] -> Group
  go curr _ []       = curr
  go curr g (pos : ps) 
    | pos `S.notMember` curr = 
      let 
        newPositions = 
          [ move pos 
          | move <- directions, move pos `S.notMember` curr
          , let newPos = g M.!? move pos, isJust newPos
          , newPos == g M.!? pos
          ]
      in go (S.insert pos curr) g (newPositions ++ ps)
    | otherwise = go curr g ps

lengthEdge :: Eq a => Group -> Grid a -> ((Int, Int) -> (Int, Int)) -> Int
lengthEdge group grid direction =
    length [ () | p <- S.toList group, let a = grid M.! p, let mb = grid M.!? direction p, isNothing mb || fromJust mb /= a ]

solve1 :: Eq a => Grid a -> Int
solve1 g = sum [ S.size group * sum (map (lengthEdge group g) directions) | group <- separateGroups g ]

type Pattern = [(Bool, (Int, Int))]

outerCorner :: Pattern
outerCorner = [(False, (1, 0)), (False, (0, 1))]

innerCorner :: Pattern
innerCorner = [(True, (1, 0)), (True, (0, 1)), (False, (1, 1))]

rotate :: Pattern -> Pattern
rotate = map (\(b, (x, y)) -> (b, (-y, x)))

corners :: [Pattern]
corners = take 4 (iterate rotate outerCorner) ++ take 4 (iterate rotate innerCorner)  

match :: Group -> (Int, Int) -> Pattern -> Bool
match g (px, py) = all (\(b, (dx, dy)) -> (px + dx, py + dy) `S.member` g == b) 

countCorners :: Group -> Int
countCorners g = length $ filter id (match g <$> S.toList g <*> corners)

solve2 :: Eq a => Grid a -> Int
solve2 g = sum [ S.size group * countCorners group | group <- separateGroups g ]