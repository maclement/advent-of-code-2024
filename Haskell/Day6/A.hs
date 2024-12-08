{-# LANGUAGE LambdaCase #-}
import qualified Data.Map as M
import Data.Bifunctor (first, second)
import Control.Monad.State.Lazy (gets, modify, evalState, execState, State)
import Data.List (nub)


type Coordinate = (Int, Int)
type Grid = M.Map Coordinate Char

data Direction = North | East | South | West
  deriving Eq

main1, main2 :: IO ()
main1 = readFile "./input.txt" >>= print . solve1 . toGrid . lines
main2 = readFile "./input.txt" >>= print . solve2 . toGrid . lines

toGrid :: [String] -> Grid
toGrid input = M.fromList
  [ ((y, x), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]


data Info = Info { curPos :: Coordinate, curDir :: Direction, grid :: Grid, visited :: [(Coordinate, Direction)] }

-- | Translates direction to step function
move :: Direction -> Coordinate -> Coordinate
move = \case
  North -> first pred
  East  -> second succ
  South -> first succ
  West  -> second pred

-- | Rotates direction
rotate :: Direction -> Direction
rotate = \case
  North -> East
  East  -> South
  South -> West
  West  -> North

updateInfo :: Coordinate -> Direction -> Info -> Info
updateInfo newPos dir (Info p d g v) = Info newPos d g ((newPos, dir) : v)

solve1 :: Grid -> Int 
solve1 g = let (startPos, c) = head $ M.toList $ M.filter (\x -> x /= '.' && x /= '#') g
               dir           = case c of
                '^' -> North
                '>' -> East
                'v' -> South
                '<' -> West
               initialState  = Info startPos dir g [(startPos, dir)]
               result        = execState trapped initialState
           in length $ nub $ map fst $ visited result

-- | Tries to perform a single step and rotates if infront of a wall
trapped :: State Info Bool
trapped = do
  pos  <- gets curPos
  dir  <- gets curDir
  g    <- gets grid
  v    <- gets visited 
  let newPos = move dir pos
  if (newPos, dir) `elem` v 
    then return True 
    else do
      case g M.!? newPos of
        Nothing  -> return False
        Just '#' -> modify (\s -> s{curDir = rotate dir}) >> trapped
        Just _   -> modify (updateInfo newPos dir) >> trapped

solve2 :: Grid -> Int
solve2 g = let (startPos, c) = head $ M.toList $ M.filter (\x -> x /= '.' && x /= '#') g
               dir           = case c of
                '^' -> North
                '>' -> East
                'v' -> South
                '<' -> West
               result        = execState trapped (Info startPos dir g [(startPos, dir)])
           in length [ obstPos 
                     | obstPos <- nub $ map fst $ visited result
                     , obstPos /= startPos
                     , let g' = M.insert obstPos '#' g
                     , let initialState = Info startPos dir g' [(startPos, dir)]
                     , evalState trapped initialState 
                     ]