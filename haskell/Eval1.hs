module Eval1 where

-- 抽象構文木
data Expr = Int Int
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
                
-- 評価器
eval :: Expr -> Either String Int
eval (Int n) = Right n
eval (Add e1 e2) = lifte (+) (eval e1) (eval e2)
eval (Sub e1 e2) = lifte (-) (eval e1) (eval e2)
eval (Mul e1 e2) = lifte (*) (eval e1) (eval e2)
eval (Div e1 e2)
  | v2 == Right 0 = Left "fail"
  | otherwise = lifte div (eval e1) v2
  where v2 = eval e2

lift :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
lift f (Just x) (Just y) = Just (x `f` y)
lift _ _ _ = Nothing

--lifte :: (a -> a -> a) -> Either
lifte f (Right x) (Right y) = Right (x `f` y)
lifte _ _ _ = Left "Fail"
-- テスト
test1 = Add (Int 2) (Int 3)
test2 = Sub (Int 2) (Div (Int 10) (Int 2))
test = Div (Int 1) (Int 0)
st = Div (Div (Int 1) (Int 0)) (Int 1)