import Data.List (sortBy, sort, groupBy)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as S

main1, main2 :: IO ()
main1 = readFile "./input.txt" >>= print . solve1 . toInput . lines
main2 = readFile "./input.txt" >>= print . solve2 . toInput . lines

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred xs = let (x, ys) = span pred xs
                    in x : splitWith pred (drop 1 ys)

toInput :: [[Char]] -> ([(Int, Int)], [[Int]])
toInput input = 
  let parts        = splitWith (not . null) input
      updatesSplit = map (map (read @Int) . splitWith (/= ',')) (parts !! 1)
  in ( 
       [ (read $ head x, read $ x !! 1 ) 
       | line <- head parts
       , let x = splitWith (/= '|') line 
       ]
     , updatesSplit
     )

inOrder :: [(Int, Int)] -> [Int] -> Bool
inOrder _         []     = True
inOrder openrules (x:xs) | x `elem` snds = False
                         | x `elem` fsts = inOrder (filter ((/= x) . fst) openrules) xs
                         | otherwise     = inOrder openrules xs
 where (fsts, snds) = unzip openrules

solve1 :: ([(Int, Int)], [[Int]]) -> Int
solve1 (rules, inputs) = sum
      [ input !! (length input `div` 2)
      | input <- inputs
      , let rules' = filter (\(x,y) -> x `elem` input && y `elem` input) rules
      , inOrder rules' input
      ]

solve2 :: ([(Int, Int)], [[Int]]) -> Int
solve2 (rules, inputs) = sum
      [ result
      | input <- inputs
      , let rules' = filter (\(x,y) -> x `elem` input && y `elem` input) rules
      , not (inOrder rules' input)
      , result <- input
      , countPreds rules' result == length input `div` 2
      ]

countPreds :: [(Int, Int)] -> Int -> Int
countPreds rules x = length (filter ((== x) . snd) rules)