main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . length . filter id . map (safe. readLine) . lines
main2 = readFile "./input.txt"  >>= print . length . filter id . map (safeTolerate . readLine) . lines

readLine :: String -> [Int]
readLine = map read . splitWith (/= ' ')

splitWith :: (Char -> Bool) -> [Char] -> [[Char]]
splitWith pred [] = []
splitWith pred xs = let (x, ys) = span pred xs
                    in x : splitWith pred (drop 1 ys)

safe :: [Int] -> Bool
safe xs = absdist' xs || absdist' (reverse xs)

absdist' :: [Int] -> Bool
absdist' xs = all (\a -> a <= 3 && a >= 1) $ zipWith (-) (tail xs) xs 

safeTolerate :: [Int] -> Bool
safeTolerate xs = safe xs || or [ safe (as ++ drop 1 bs) | i <- [0 .. length xs], let (as, bs) = splitAt i xs ]