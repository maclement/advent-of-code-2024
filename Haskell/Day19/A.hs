import Control.Monad.State (gets, State, modify, runState, evalState)
import qualified Data.Map as M
import Data.Bifunctor (second)
import Data.List (isPrefixOf)

main1, main2 :: IO ()
main1 = readFile "./input.txt" >>= print . uncurry solve1 . toInput . lines
main2 = readFile "./input.txt" >>= print . uncurry solve2 . toInput . lines

toInput :: [String] -> ([String], [String])
toInput s = let (ps, xs) = break null s
            in (map (filter (/=',')) $ words (head ps), drop 1 xs)

solve1 :: [String] -> [String] -> Int
solve1 pieces towels =
 let res = evalState (mapM buildPattern towels) (pieces, M.singleton "" 1)
 in length $ filter (>0) res

solve2 :: [String] -> [String] -> Int
solve2 pieces towels =
 let (b, m) = runState(mapM buildPattern towels) (pieces, M.singleton "" 1)
 in sum (map (`div` snd m M.! "") b) 

buildPattern :: String -> State ([String], M.Map String Int) Int
buildPattern s =
  gets ((M.!? s) . snd) >>=
  maybe (do
    b <- (sum <$>) . mapM (tryMatch s) =<< gets fst
    modify (second (M.insert s b))
    return b) 
    return

tryMatch :: String -> String -> State ([String], M.Map String Int) Int
tryMatch [] _     = return 1
tryMatch s  piece | piece `isPrefixOf` s = buildPattern (drop (length piece) s)
                  | otherwise = return 0