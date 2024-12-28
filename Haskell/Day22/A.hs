import Data.Bits (xor)
import qualified Data.Map as M

type Group = (Int, Int, Int, Int)
type Log = M.Map Group Int

main1, main2 :: IO ()
main1 = readFile "./input.txt" >>= print . solve1 . map read . lines
main2 = readFile "./input.txt" >>= print . solve2 . map read . lines

nextNumber :: Int -> Int
nextNumber s = let s'  = prune $ (s * 64) `xor` s
                   s'' = prune $ (s' `div` 32) `xor` s'
               in prune $ (s'' * 2048) `xor` s''
 where prune = (`mod` 16777216)

iterateN :: (Int -> Int) -> Int -> Int -> [Int]
iterateN f n base = take n $ iterate f base

solve1 :: [Int] -> Int
solve1 = sum . map (last . iterateN nextNumber 2000)

logChanges :: [Int] -> M.Map (Int, Int, Int, Int) Int
logChanges snums = let digits = map (`mod` 10) snums
                       gdiffs = slice4 $ changes digits
                       slices = gdiffs `zip` drop 4 digits
                   in foldr (uncurry M.insert) M.empty slices

changes :: [Int] -> [Int]
changes xs = zipWith (-) (drop 1 xs) xs

slice4 :: [Int] -> [Group]
slice4 xs | length xs >= 4 = let [a, b, c, d] = take 4 xs
                             in go (a, b, c, d)  (drop 4 xs)
          | otherwise      = []
 where
  go y                  [] = [y]
  go (a, b, c, d) (y : ys) = (a, b, c, d) : go (b, c, d, y) ys

solve2 :: [Int] -> Int
solve2 = maximum . M.unionsWith (+) . map (logChanges . iterateN nextNumber 2000)