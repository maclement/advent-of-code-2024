{-# LANGUAGE LambdaCase #-}

import Control.Monad.State ( execState, gets, modify, State )
import Data.Bifunctor (first, second)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as M
import qualified Data.Set as S

type Grid = M.Map (Int, Int) Char

data Direction = N | E | S | W
 deriving (Eq, Ord, Enum, Show)

type Position = (Int, Int, Direction)

data Data = Data { grid    :: Grid
                 , visited :: M.Map Position Int
                 , queue   :: S.Set (Int, Direction, (Int, Int))
                 , parent  :: M.Map Position [Position]
                 }

toGrid :: [String] -> Grid
toGrid input = M.fromList $
   [ ((y, x), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]

dijkstra :: State Data ()
dijkstra = do
  q <- gets queue
  vis <- gets visited
  if S.null q
    then return ()
    else do
      let ((dist, dir, (currX, currY)), q') = S.deleteFindMin q
      if isJust (vis M.!? (currX, currY, dir))
        then do modify (\s -> s { queue = q' }) >> dijkstra
        else do
          ns <- neighboors dist (currX, currY) dir
          let notVisited = filter (\(_, fd, (fx, fy)) -> isNothing (vis M.!? (fx, fy, fd))) ns
          let notVisitedReordered = [ ((p1, p2, d), i) | (i, d, (p1, p2)) <- notVisited]
          let visitedCurrOpt = [ (p1, p2, dir) | (score, d, (p1, p2)) <- ns, maybe False (== score) (vis M.!? (p1, p2, d))]
          modify $ \s -> s { visited = M.insert (currX, currY, dir) dist (visited s)
                           , queue   = foldr S.insert q' notVisited
                           , parent  = foldl (updateParent (currX, currY, dir)) (parent s) $ map fst notVisitedReordered ++ visitedCurrOpt
                           }
          dijkstra

updateParent :: Position -> M.Map Position [Position] -> Position -> M.Map Position [Position]
updateParent par m des | isJust (m M.!? des) = M.update (Just . (par :)) des m
                       | otherwise = M.insert des [par] m

neighboors :: Int -> (Int, Int) -> Direction -> State Data [(Int, Direction, (Int, Int))]
neighboors dist pos dir = do
  g <- gets grid
  let rotateLeft = rotate . rotate . rotate $ dir
  let rotateRight = rotate dir
  return $ filter (\(_,_,x) -> maybe False (/='#') (g M.!? x)) [(dist + 1000, rotateLeft , pos), (dist + 1000, rotateRight, pos), (dist + 1, dir, toMove dir pos)]

rotate :: Direction -> Direction
rotate = \case
  N -> E
  E -> S
  S -> W
  W -> N

toMove :: Direction -> ((Int, Int) -> (Int, Int))
toMove = \case
 N -> first pred
 E -> second succ
 S -> first succ
 W -> second pred

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve1 . toGrid . lines
main2 = readFile "./input.txt"  >>= print . solve2 . toGrid . lines

solve1 :: Grid -> Int
solve1 g = let (startX, startY) = fst $ head $ M.toList $ M.filter (== 'S') g
               endPos     = fst $ head $ M.toList $ M.filter (== 'E') g
               inputData  = Data g M.empty (S.singleton (0, E, (startX, startY))) M.empty
               resultData = execState dijkstra inputData
               res        = [ dist | ((x, y, _), dist) <- M.toList (visited resultData), (x,y) == endPos ]
           in minimum res

solve2 :: M.Map (Int, Int) Char -> Int
solve2 g = let (startX, startY) = fst $ head $ M.toList $ M.filter (== 'S') g
               (endX, endY)     = fst $ head $ M.toList $ M.filter (== 'E') g
               inputData  = Data g M.empty (S.singleton (0, E, (startX, startY))) M.empty
               resultData = execState dijkstra inputData
               res        = minimum [ dist | ((x, y, _), dist) <- M.toList (visited resultData), (x, y) == (endX, endY) ]
               chains     = foldr1 S.union [ computePaths (parent resultData) (startX, startY, E) (endX, endY, dir)
                                           | dir <- [N ..]
                                           , let ms = visited resultData M.!? (endX, endY, dir)
                                           , maybe False (== res) ms
                                           ]
           in S.size chains    

computePaths ::  M.Map Position [Position] -> Position -> Position -> S.Set (Int, Int)
computePaths m startPos goalPos@(goalPosX, goalPosY, _) | startPos == goalPos = S.singleton (goalPosX, goalPosY)
computePaths m startPos goalPos@(goalPosX, goalPosY, _) =
  S.singleton (goalPosX, goalPosY) `S.union` foldr1 S.union (map (computePaths m startPos) (m M.! goalPos))