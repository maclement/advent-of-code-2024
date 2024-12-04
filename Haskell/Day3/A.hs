import Control.Applicative
import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Text.Parsec (try, eof)
import Text.Parsec.Char (string, char, digit, anyChar)
import Text.Parsec.Combinator (between)
import Text.Parsec.String (Parser, parseFromFile)

main1, main2 :: IO ()
main1 = parseFromFile (parser instruction) "./input.txt" >>= print
main2 = parseFromFile (parser instructionsCond) "./input.txt" >>= print

parser :: Parser (Maybe Int) -> Parser Int
parser p = sum . catMaybes <$> many p

instruction :: Parser (Maybe Int)
instruction =  try (string "mul" >> Just . uncurry (*) <$> numberPair)
           <|> Nothing <$ anyChar

numberPair :: Parser (Int, Int)
numberPair = parens $ (,) <$> number <*> (char ',' *> number)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

number :: Parser Int
number = read <$> sat ((<=3) . length) (some digit)

sat :: (a -> Bool) -> Parser a -> Parser a
sat p pa = do
  a <- pa
  guard $ p a
  return a

instructionsCond :: Parser (Maybe Int)
instructionsCond = try (string "mul" >> Just . uncurry (*) <$> numberPair)
           <|> try (string "don't()") *> untilDo
           <|> Nothing <$ anyChar

untilDo :: Parser (Maybe a)
untilDo = try (Nothing <$ string "do()") 
       <|> anyChar *> untilDo
       <|> Nothing <$ eof