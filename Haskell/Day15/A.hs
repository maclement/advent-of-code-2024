{-# Language LambdaCase #-}

import Data.Bifunctor (first, second)
import qualified Data.Map as M
import Control.Monad (when)
import Control.Monad.State.Lazy
import Data.List (maximumBy, nub)
import Data.Function (on)
import Debug.Trace


type Grid = M.Map (Int, Int) Char
type Instruction = (Int, Int) -> (Int, Int)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve1 . toInput . lines
main2 = readFile "./input.txt"  >>= print . solve2 . toInput . lines

printResult :: IO ()
printResult = readFile "./input.txt"  >>= execAndVisualize . toInput . lines

toInstruction :: Char -> Instruction
toInstruction = \case
 '^' -> first pred
 '>' -> second succ
 'v' -> first succ
 '<' -> second pred

toGrid :: [String] -> Grid
toGrid input = M.fromList
  [ ((y, x), char) | (y, row ) <- zip [0..] input, (x, char) <- zip [0..] row ]

toInput :: [String] -> (Grid, [Instruction])
toInput input = let (gs, rs) = break null input
                in (toGrid gs, map toInstruction (concat $ tail rs))

solve1 :: (Grid, [Instruction]) -> Int
solve1 (g, is) = let startPos = fst $ head $ M.toList $ M.filter (== '@') g
                 in computeScore 'O' $ grid $ execState (performInstructions is) (Board startPos g)

computeScore :: Char -> Grid -> Int
computeScore c = sum . map ((\(x,y) -> x * 100 + y) . fst) . M.toList . M.filter (== c)

data Board = Board { pos :: (Int, Int), grid :: Grid }

performInstructions :: [Instruction] -> State Board ()
performInstructions = foldr (\ i -> (>>) (gets pos >>= flip tryMove i)) (return ())

tryMove :: (Int, Int) -> Instruction -> State Board (Bool, [(Int, Int)])
tryMove pos inst = do
  c <- gets ((M.! pos) . grid)
  case c of
    '#' -> return (False, [])
    '.' -> return (True, [])
    'O' -> do
      fmap (pos :) <$> tryMove (inst pos) inst
    '@' -> do
      (b, us) <- tryMove (inst pos) inst
      when b (modify (updatePlayer inst pos (pos : us)))
      return (b, [])
    '[' -> case inst (0, 0) of
      (0, _) -> fmap (pos :) <$> tryMove (inst pos) inst
      (_, 0) -> 
        fmap ([pos, second succ pos] ++) 
          <$> (combineUpdates <$> tryMove (inst pos) inst 
                              <*> tryMove (second succ $ inst pos) inst)
    ']' -> case inst (0, 0) of
      (0, _) -> fmap (pos :) <$> tryMove (inst pos) inst
      (_, 0) -> 
        fmap ([pos, second pred pos] ++) 
          <$> (combineUpdates <$> tryMove (inst pos) inst 
                              <*> tryMove (second pred $ inst pos) inst)


combineUpdates :: (Bool, [(Int, Int)]) -> (Bool, [(Int, Int)]) -> (Bool, [(Int, Int)])
combineUpdates (b1, u1) (b2, u2) = (b1 && b2, u1 ++ u2)

updatePlayer :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> [(Int, Int)] -> Board -> Board
updatePlayer dir pos updates (Board _ g) = 
  let singUpdates = nub updates
      x = foldr (flip M.insert '.') g singUpdates
  in Board (dir pos) $ foldr (\p -> M.insert (dir p) (g M.! p)) x singUpdates

solve2 :: (Grid, [Instruction]) -> Int
solve2 (g, is) = 
  let startPos = fst $ head $ M.toList $ M.filter (== '@') (transformGrid g)
  in computeScore '[' $ grid $ execState (performInstructions is) (Board startPos (transformGrid g))

transformGrid :: Grid -> Grid
transformGrid g = let cells = M.toList g
                      transformed = map (second transform) cells
                      shifted = shiftInput transformed
                  in M.fromList shifted

shiftInput :: [((Int, Int), (Char, Char))] -> [((Int, Int), Char)]
shiftInput [] = []
shiftInput (((x, y), (c1, c2)) : r) = ((x, 2 * y), c1) : ((x, 2 * y + 1), c2) : shiftInput r

transform :: Char -> (Char, Char)
transform = \case
  '#' -> ('#', '#')
  'O' -> ('[', ']')
  '.' -> ('.', '.')
  '@' -> ('@', '.')

gridToStrings :: Grid -> [String]
gridToStrings g = let colMax = fst $ maximumBy (compare `on` fst) (M.keys g)
                      rowMax = snd $ maximumBy (compare `on` snd) (M.keys g)     
                  in [ [ m | col <- [0 .. rowMax], let m = g M.! (row, col) ] | row <- [0..colMax] ]

printGrid :: Grid -> IO ()
printGrid = mapM_ print . gridToStrings

execAndVisualize :: (Grid, [Instruction]) -> IO ()
execAndVisualize (g, is) = let startPos = fst $ head $ M.toList $ M.filter (== '@') (transformGrid g)
                           in printGrid $ grid $ execState (performInstructions is) (Board startPos (transformGrid g))