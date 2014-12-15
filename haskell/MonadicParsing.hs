module MonadicParse where

{-
  Graham Hutton and Erik Meijer, Monadic parsing in Haskell. JFP 1998
-}

import Data.Char
--import Control.Monad
--import Control.Applicative

newtype Parser a = Parser (String -> [(a,String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                  "" -> []
                  (c:cs) -> [(c,cs)])
       

parse (Parser p) = p

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])


class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (++.) :: m a -> m a -> m a

instance MonadZero Parser where
  zero = Parser (\cs -> [])

instance MonadPlus Parser where
  p ++. q = Parser (\cs -> parse p cs ++ parse q cs)

-- Control.MonadのMonadPlusを使っても良い

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++. q) cs of
                     [] -> []
                     (x:xs) -> [x])

-- p(redicate)が真になるときのみreturn
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser Char
char c = sat (c ==)

--let p1 = char 'a' ++. char 'A' --'a' or 'A'にマッチする

opParser c f = do
  x <- sat isDigit
  char c
  y <- sat isDigit
  return (digitToInt x `f` digitToInt y)

addParser = opParser '+' (+)
mulParser = opParser '*' (*)

exprParser = addParser ++. mulParser

test = (char 'a' +++ sat isAlpha)

-- 論文(Functional pearl)にもどる
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c ; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

--pint = do {xs <- many1 
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby` sep) +++ return []
sepby1 :: Parser a -> Parser b -> Parser [a]
p`sepby1` sep = do a <- p
                   as <- many (do {sep; p})
                   return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where
    rest a = (do f <- op
                 b <- p
                 rest (f a b)) +++ return a
             
