module Ex215 where

{-
 Exercise 2.15でつくったEvaluator
 evalN で評価するとCall by Name
 evalV で評価するとCall by Value
-}

-- つくったParser
import PCFParser 


binop Mul = (*)
binop Div = div
binop Add = (+)
binop Sub = (-)

-- 代入 (u/key)t
subst :: Id -> Expr -> Expr -> Expr
subst key t u = sub t
  where
    sub t'@(Var x) = if x == key then u else t'
    sub t'@(Fun x p) = if x == key then t' else Fun x (sub p)
    sub (App p q) = App (sub p) (sub q)
    sub (BinOp op p q) = BinOp op (sub p) (sub q)
    sub (Ifz p q r) = Ifz (sub p) (sub q) (sub r)
    sub t'@(Fix x p) = if x == key then t' else Fix x (sub p)
    sub t'@(Let x p q) = if x == key then t' else Let x (sub p) (sub q)
    sub t' = t' --それ以外はそのまま

-------------------------
-- Call by Nameの評価器
-------------------------
evalN :: Expr -> Expr
evalN (App t u) =
  case evalN t of
    Fun x t' -> evalN $ subst x t' u
    _ -> error "Applying to not-function"
evalN t'@(Fun x t) = t'
evalN t@(Var x) = t
evalN t@(Int n) = t
evalN (BinOp op t u) = Int $ if ans < 0 then 0 else ans 
  where
    ans = binop op p q
    p = toInt $ evalN t
    q = toInt $ evalN u
    toInt (Int n) = n
evalN (Ifz t u v) =
  case evalN t of
    Int 0 -> evalN u
    Int _ -> evalN v
    _ -> error "condition is not number"
evalN (Fix x t) = evalN $ subst x t (Fix x t)
evalN (Let x t u) = evalN $ subst x u t

---------------------------
-- Call by Valueの評価器
---------------------------
evalV :: Expr -> Expr
evalV (App t u) = w `seq` case evalV t of --seq a bはaを簡約してからbにいく
  Fun x t' -> evalV $ subst x t' w
  where w = evalV u
evalV (Let x t u) = w `seq` evalV $ subst x u w
  where w = evalV t
-- 以下CBNと同じ
evalV t'@(Fun x t) = t'
evalV t@(Var x) = t
evalV t@(Int n) = t
evalV (BinOp op t u) = Int $ if ans < 0 then 0 else ans
  where
    ans = binop op p q
    p = toInt $ evalV t
    q = toInt $ evalV u
    toInt (Int n) = n
evalV (Ifz t u v) =
  case evalV t of
    Int 0 -> evalV u
    Int _ -> evalV v
    _ -> error "condition is not number"
evalV (Fix x t) = evalV $ subst x t (Fix x t)

  
-- 実行例
fact6 = do
  p <- runParse "let fact = fix f (fun x -> ifz x then 1 else x * f (x-1)) in fact 6"
  putStr "evaluate fact 6\nCBN : "
  print $ evalN p
  putStr "CBV : "
  print $ evalV p

cb1 = do
  p <- runParse "let c = fun x -> 0 in let b1 = (fix f (fun x -> f x)) 0 in c b1"
  putStr "evaluate C b1\nCBN :"
  print $ evalN p
  putStr "CBV : "
  print $ evalV p

fib = do
  p <- runParse "let fib = fix f (fun x -> ifz x then 1 else ifz x-1 then 1 else f (x-1) + f (x-2)) in fib 20"
  print $ evalN p

tarai = do
  p <- runParse "let tarai = fix f (fun x -> fun y -> fun z -> ifz (x-y) then y else f (f (x-1) y z) (f (y-1) z x) (f (z-1) x y)) in tarai 10 5 0"
  putStr "tarai 10 5 0 in Evaluator"
  print $ evalN p