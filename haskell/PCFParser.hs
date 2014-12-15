module PCFParser where

{-
 Simple PCF Paser written in monadic and applicative style
 入力例: let fact = fix f (fun x -> ifz x then 1 else x * f (x-1)) in fact 6
-}

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)


type Id = String
data Expr = Var Id
          | Fun Id Expr
          | App Expr Expr
          | Int Int
          | BinOp BinOp Expr Expr
          | Ifz Expr Expr Expr
          | Fix Id Expr
          | Let Id Expr Expr
          deriving (Show, Eq, Ord)

data BinOp = Add | Sub | Mul | Div
           deriving (Show, Eq, Ord)

-- パージングを実行する
runParse :: String -> IO Expr
runParse s = case parse parseMain "" s of
  Left err -> fail $ show err
  Right e -> return e

parseMain = spaces *> expr <* eof

-- 小規模だからこの関数一つに全部ぶら下げる
expr :: Parser Expr
expr = funExpr
       <|> ifzExpr
       <|> fixExpr
       <|> letExpr
       <|> binOpExpr
  where
    -- 便利関数
    ident = (:) <$> letter <*> many (alphaNum <|> char '_') <* spaces
    keywd s = try $ spaces *> (string s) <* spaces
    reserved = ["fun","fix","let","in","ifz","then","else"] 

    -- 本体
    funExpr = Fun <$> (keywd "fun" *> ident) <*> (keywd "->" *> expr)
    ifzExpr = Ifz <$> (keywd "ifz" *> expr) <*> (keywd "then" *> expr)
              <*> (keywd "else" *> expr)
    fixExpr = Fix <$> (keywd "fix" *> ident) <*> expr
    letExpr = Let <$> (keywd "let" *> ident) <*> (keywd "=" *> expr)
              <*> (keywd "in" *> expr)
    binOpExpr = mulExpr `chainl1` addOp
      where
        mulExpr = appExpr `chainl1` mulOp
        appExpr = factor `chainl1` (return App)

        addOp = keywd "+" *> pure (BinOp Add)
                <|> keywd "-" *> pure (BinOp Sub)
        mulOp = keywd "*" *> pure (BinOp Mul)
                <|> keywd "/" *> pure (BinOp Div)
        factor = paren <|> int <|> var
          where
            paren = keywd "(" *> expr <* keywd ")"
            int = Int . read <$> (spaces *> many1 digit <* spaces)
            var = try $ do
              x <- ident
              if (elem x reserved) then parserZero else return (Var x)
    
