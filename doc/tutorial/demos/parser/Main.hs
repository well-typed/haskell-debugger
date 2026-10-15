module Main where

import Data.Char (isDigit)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

char :: Char -> Parser Char
char c = Parser $ \s -> case s of
  (x : rest) | x == c -> Just (c, rest)
  _                   -> Nothing

digit :: Parser Int
digit = Parser $ \s -> case s of
  (x : rest) | isDigit x -> Just (read [x], rest)
  _                      -> Nothing

orElse :: Parser a -> Parser a -> Parser a
orElse p q = Parser $ \s ->
  case runParser p s of
    Just r  -> Just r
    Nothing ->
      runParser q s          -- logpoint: "backtracking on {s}"
                             -- or hit count: >= 1000

andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen p f = Parser $ \s -> case runParser p s of
  Nothing        -> Nothing
  Just (a, rest) -> runParser (f a) rest

-- expr := term '+' expr | term
-- term := '(' expr ')' | digit
expr :: Parser Int
expr = (term `andThen` \a -> char '+' `andThen` \_ -> expr `andThen` \b -> pure' (a + b))
       `orElse` term

term :: Parser Int
term = (char '(' `andThen` \_ -> expr `andThen` \e -> char ')' `andThen` \_ -> pure' e)
       `orElse` digit

pure' :: a -> Parser a
pure' a = Parser $ \s -> Just (a, s)

main :: IO ()
main = mapM_ (\n -> print (runParser expr (nested n))) [1 .. 25]
  where nested n = replicate n '(' ++ "1" ++ replicate n ')'
