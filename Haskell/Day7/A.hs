import Data.Maybe (mapMaybe, listToMaybe)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . sum . map fst . filter (uncurry checkEquation) . map toInput . lines
main2 = readFile "./input.txt"  >>= print . sum . map fst . filter (uncurry checkEquationWithConcat) . map toInput . lines

toInput :: String -> (Integer, [Integer])
toInput s = let (res:xs) = words s
            in (read $ take (length res - 1) res, map read xs)

checkEquation :: Integer -> [Integer] -> Bool
checkEquation res (i:is) = checkEquation' i res is  
 where
  checkEquation' curr res []      = res == curr
  checkEquation' curr res (n:ns)  = checkEquation' (n * curr) res ns  || checkEquation' (n + curr) res ns

checkEquationWithConcat :: Integer -> [Integer] -> Bool
checkEquationWithConcat res (i:is) = checkEquation' i res is  
 where
  checkEquation' curr res []      = res == curr
  checkEquation' curr res (n:ns)  = checkEquation' (n * curr) res ns  
                                 || checkEquation' (n + curr) res ns 
                                 || checkEquation' (read @Integer (show curr ++ show n)) res ns  -- i feel bad for writing this