{-# LANGUAGE DataKinds, DerivingVia, ScopedTypeVariables #-}
import Data.Bifunctor (second, bimap)
import Data.Function (on)
import Data.Functor (void)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Text.Parsec (Parsec, char, digit, newline, string, eof, many1, option, (<|>))
import Text.Parsec.String (parseFromFile)
import Data.List (sortBy, minimumBy)

data Robot w h = Robot { pos :: (Z w, Z h), vel :: (Z w, Z h) }
  deriving Show

-- Read should be implemented with normalization in mind, same with fromInteger
newtype Z (n :: Nat) = Zn { unZn :: Integer }
  deriving (Show, Read, Eq, Ord) via Integer

instance KnownNat n => Num (Z n) where
  zn1 + zn2 = Zn $ (unZn zn1 + unZn zn2) `mod` natVal (Proxy @n)
  zn1 * zn2 = Zn $ (unZn zn1 * unZn zn2) `mod` natVal (Proxy @n)
  fromInteger i = Zn (i `mod` natVal (Proxy @n))
  negate (Zn i) = Zn (natVal (Proxy @n) - i) 

instance (Num a, Num b) => Num (a, b) where 
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  fromInteger n = (fromInteger n, fromInteger n)

step :: (KnownNat w, KnownNat h) => Integer -> Robot w h -> Robot w h
step n r = r{pos = pos r + fromInteger n * vel r}

robot :: (KnownNat w, KnownNat h) => Parsec String () (Robot w h)
robot = 
  Robot 
    <$> ((,) <$> (string "p=" *> number ) 
             <*> (string ","  *> number <* char ' ')
        )
    <*> ((,) <$> (string "v=" *> number) 
             <*> (string "," *> number <* (void newline <|> eof)))
  where
    number :: (KnownNat n) => Parsec String () (Z n)
    number = decodeSign <$> option id (negate <$ string "-") <*> many1 digit

    decodeSign :: (Num a, Read a) => (a -> a) -> String -> a
    decodeSign s = s . read

main1 :: IO ()
main1 = parseFromFile (many1 robot) "./input.txt" >>= print . either show (show . solve)

solve :: [Robot 101 103] -> Int
solve r = product $ map length $ splitQuadrants $ [step a r' | a <- [100], r' <- r] 

splitQuadrants :: (KnownNat w, KnownNat h) => [Robot w h] -> [[Robot w h]]
splitQuadrants rs = let (left, right) = splitVertical rs
                        (leftUp, leftDown) = splitHorizontal left
                        (rightUp, rightDown) = splitHorizontal right
                    in [leftUp, leftDown, rightUp, rightDown]

splitVertical :: forall w h. KnownNat w => [Robot w h] -> ([Robot w h], [Robot w h])
splitVertical = second (dropWhile $ \r -> unZn (fst (pos r)) == (natVal (Proxy @w) `div` 2)) 
  . span (\r -> unZn (fst (pos r)) < (natVal (Proxy @w) `div` 2)) 
  . sortBy (compare `on` fst . pos)

splitHorizontal :: forall w h. KnownNat h => [Robot w h] -> ([Robot w h], [Robot w h])
splitHorizontal = second (dropWhile $ \r -> unZn (snd (pos r)) == (natVal (Proxy @h) `div` 2)) 
  . span (\r -> unZn (snd (pos r)) < (natVal (Proxy @h) `div` 2)) 
  . sortBy (compare `on` snd . pos)

findElem :: (Integer, Integer) -> [Robot w h] -> String
findElem p r  = let l = length [ () | r' <- r, bimap unZn unZn (pos r') == p] in if l == 0 then "." else show l

printBoard :: [Robot 101 103] -> IO ()
printBoard r = mapM_ print [ concat [ x | w <- [0..100], let x = findElem (w, h) r ] | h <- [0..102]]

main2:: IO ()
main2 = do
  ea <- parseFromFile (many1 robot) "./input.txt"
  case ea of
    Left _  -> return ()
    Right (r :: [Robot 101 103]) -> do
      let sols = [(r', a, product $ map length $ splitQuadrants r') | a <- [1..10000], let r' = map (step a) r]
      let (lowestScore, iteration, _) = minimumBy (compare `on` (\(_,_,z) -> z)) sols 
      printBoard lowestScore
      print $ "The correct iteration is: " ++ show iteration