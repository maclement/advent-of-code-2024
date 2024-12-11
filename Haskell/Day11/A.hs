import Control.Monad.State.Lazy
import Control.Monad (mapM)
import Data.Map (Map)
import qualified Data.Map as M

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve' 25 . map read . words . head . lines
main2 = readFile "./input.txt"  >>= print . solve' 75 . map read . words . head . lines

solve' :: Int -> [Int] -> Int
solve' i input = sum $ execState (mapM (flip iterateNTimes i) input) M.empty

-- | Iterates the rules a given amount of times and collects the resulting sum of elements.
-- | Stores results in a map. Haskell is my favorite imperative programming language (:
iterateNTimes :: Int -> Int -> State (Map (Int, Int) Int) Int
iterateNTimes n 0          = modify (M.insert (n, 0) 1) >> return 1
iterateNTimes n iterations = do
    m <- gets (M.!? (n, iterations))
    case m of
      Just i                       -> return i
      _ | n == 0                   -> do
          res <- iterateNTimes 1 (iterations - 1) 
          modify (M.insert (n, iterations) res)
          return res
        | even . length . show $ n -> do 
          let n1 = read $ take (length (show n) `div` 2) (show n) 
          let n2 = read $ drop (length (show n) `div` 2) (show n)
          res <- (+) <$> iterateNTimes n1 (iterations - 1) <*> iterateNTimes n2 (iterations - 1)
          modify (M.insert (n, iterations) res)
          return res
        | otherwise                -> do 
            res <- iterateNTimes (2024 * n) (iterations - 1)
            modify (M.insert (n, iterations) res)
            return res