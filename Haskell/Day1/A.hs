import Data.List (sort, group)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Maybe (fromMaybe)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve . unzip . map (bimap read read . span (/= ' ')) . lines
main2 = readFile "./input.txt"  >>= print . solve2 . unzip . map (bimap read read . span (/= ' ')) . lines

solve :: ([Int], [Int]) -> Int
solve (x, y) = sum $ zipWith (\a b -> abs (a - b)) (sort x) (sort y)

solve2 :: ([Int], [Int]) -> Int
solve2 (x, y) = sum $ map (fromMaybe 0 . flip lookup (map (\a -> (head a, length a * head a)) . group . sort $ y)) x