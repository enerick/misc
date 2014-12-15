module Calc where

import Data.Char

-- トークンの定義
data Token = Int Integer
           | Plus
           | Minus
           | Aster
           | Slash
           | Percent
           | LParen
           | RParen
           deriving Show

-- 字句解析
lexing :: String -> [Token]
lexing [] = []
lexing str@(c:rest)
  | isSpace c = lexing rest
  | isDigit c = Int (read s1) : lexing s2
  | '+' == c  = Plus : lexing rest
  | '-' == c  = Minus : lexing rest
  | '*' == c  = Aster : lexing rest
  | '/' == c  = Slash : lexing rest
  | '%' == c  = Percent : lexing rest
  | '(' == c  = LParen : lexing rest
  | ')' == c  = RParen : lexing rest
  | otherwise = error "不明な文字"
  where (s1,s2) = span isDigit str

-- 式
parseExpr :: [Token] -> (Integer, [Token])
parseExpr t =
  case tkns of
    Plus : rest -> (n1 + n2, tkns')
      where (n2,tkns') = parseExpr rest
    Minus : rest -> (n1 - n2, tkns')
      where (n2,tkns') = parseExpr rest
    _ -> (n1, tkns)
  where (n1,tkns) = parseTerm t

--項
parseTerm :: [Token] -> (Integer, [Token])
parseTerm t =
  case tkns of
    Aster : rest -> (n1 * n2, tkns')
      where (n2,tkns') = parseTerm rest
    Slash : rest -> (div n1 n2, tkns')
      where (n2,tkns') = parseTerm rest
    Percent : rest -> (mod n1 n2, tkns')
      where (n2,tkns') = parseTerm rest
    _ -> (n1, tkns)
  where (n1,tkns) = parseFactor t

-- 因子
parseFactor :: [Token] -> (Integer, [Token])
parseFactor n =
  case n of
    Int n : rest -> (n, rest)
    Minus : rest -> (-n, tkns)
      where (n,tkns) = parseFactor rest
    LParen : rest ->
      case tkns of
        RParen : tkns -> (n, tkns)
        _ -> error "構文エラー(閉じ括弧がない)"
      where (n,tkns) = parseExpr rest
    _ -> error "構文エラー"
    
-- 構文解析メイン
parsing :: [Token] -> Integer
parsing tkns =
  case res of
    [] -> n
    _ -> error "構文エラー"
  where
    (n, res) = parseExpr tkns

-- main
main :: IO ()
main = do
  putStr "> "
  str <- getLine
  command str

command :: String -> IO ()
command "quit" = return ()
command str = do
  print $ parsing $ lexing str
  main