module Main where

import Data.Bifunctor
import Control.Monad
import Data.Char (isDigit)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f m = Parser $ fmap (first f) . runParser m
instance Applicative Parser where
  (<*>) = ap
  pure x = Parser $ \ s -> Just (x,s)
instance Monad Parser where
  (>>=) = andThen


char :: Char -> Parser ()
char c = Parser $ \s -> case s of      -- Inline logpoint: "char {c}""
  (x : rest) | x == c -> Just ((), rest)
  _                   -> Nothing

digit :: Parser Int
digit = Parser $ \s -> case s of           -- Inline logpoint: "digit"
  (x : rest) | isDigit x -> Just (read [x], rest)
  _                      -> Nothing

orElse :: Parser a -> Parser a -> Parser a
orElse p q = Parser $ \s ->
  case runParser p s of
    Just r  -> Just r
    Nothing ->
      runParser q s          -- logpoint: "backtracking on {s}"
                             -- or hit count: >= 100

andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen p f = Parser $ \s -> case runParser p s of
  Nothing        -> Nothing
  Just (a, rest) -> runParser (f a) rest

-- expr := term '+' expr | term
-- term := '(' expr ')' | digit
expr :: Parser Int
expr = add `orElse` term
  where
    add =
      term >>=
        \ a ->
      char '+' >>    -- breakpoint here to see stack.
      expr >>=
        \ b ->
      pure (a + b)

term :: Parser Int
term = parens `orElse` digit
  where
    parens =
      char '(' >>
      expr >>= \ e ->
      char ')' >>
      pure e

main :: IO ()
main = do
    let s = nested 5
    print (runParser expr s)
  where
    nested n = replicate n '(' ++ "1" ++ replicate n ')'

















-----------------
-- Fixed parser
-----------------

-- expr' := term' ('+' expr')?
-- term' := '(' expr' ')' | digit
expr' :: Parser Int
expr' = term' >>= \a -> plusRest' a

plusRest' :: Int -> Parser Int
plusRest' a =
  (char '+' >> expr' >>= \b -> pure (a + b))
  `orElse` pure a

term' :: Parser Int
term' = (char '(' >> expr' >>= \e -> char ')' >> pure e)
        `orElse` digit




{-
-- workaround for breakpoints in user parsers: add more lambdas.

term :: Parser Int
term = parens `orElse`
   -- a breakpoint on the `pure` does break, lambda is evaluated every time.
  (digit >>= \ i -> pure i)
  where
    parens =
      char '(' >>
      expr >>= \ e ->
      char ')' >>
      pure e
-}
