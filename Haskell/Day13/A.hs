import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<|>))
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (eof, newline, many1, anyChar)
import Text.Parsec.Char (string, digit)
import Data.Bifunctor (bimap)
import GHC.Float.RealFracMethods (roundDoubleInteger)


type Button = (Integer, Integer)

main1, main2 :: IO ()
main1 = parseFromFile (many1 (parseInput <* (() <$ newline <|> eof))) "./input.txt" >>= print . either show (show . solve 0)
main2 = parseFromFile (many1 (parseInput <* (() <$ newline <|> eof))) "./input.txt" >>= print . either show (show . solve 10000000000000)

parseInput :: Parser (Button, Button, (Integer, Integer))
parseInput = (,,) <$> parsePair <*> parsePair <*> parseGoal

parseGoal :: Parser (Integer, Integer)
parseGoal =  (,) <$> (string "Prize: X=" *> parseNumber) 
                 <*> (string ", Y=" *> parseNumber ) 
                 <*  (() <$ newline <|> eof)

parsePair :: Parser (Integer, Integer)
parsePair = (,) 
         <$> (string "Button " *> anyChar *> string ": X+" *> parseNumber) 
         <*> (string ", Y+" *> parseNumber <* newline)

parseNumber :: Parser Integer
parseNumber = read <$> many1 digit

solve :: Integer -> [(Button, Button, (Integer, Integer))] -> Integer
solve i input = sum
             [ 3 * roundDoubleInteger a + roundDoubleInteger b
             | (b1, b2, p) <- input
             , let s = computeSolution b1 b2 (bimap (+ i) (+ i) p)
             , isJust s, let (a, b) = fromJust s
             , abs (a - fromIntegral (roundDoubleInteger a)) < 0.001
             , abs (b - fromIntegral (roundDoubleInteger b)) < 0.001
             ]

computeSolution :: Button -> Button -> (Integer, Integer) -> Maybe (Double, Double)
computeSolution (a, c) (b, d) (x, y)
  | invertable == 0 = Nothing
  | otherwise       = Just ((1.0 / invertable) * (fromIntegral x * fromIntegral d - fromIntegral y * fromIntegral b)
                           ,
                           (1.0 / invertable) * (fromIntegral y * fromIntegral a -  fromIntegral x * fromIntegral c))
   where
    invertable = fromIntegral $ a * d - b * c