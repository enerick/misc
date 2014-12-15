module Eval2 where

type Id = String

data Expr = Int Integer
          | Var Id
          | Fun Id Expr
          | Add Expr Expr
          | Mul Expr Expr
          | App Expr Expr
          deriving Show

data Value = INT Integer
           | FUN Id Expr Env
           deriving Show

type Env = [(Id, Value)]

subst :: Expr -> Id -> Expr -> Expr
subst (Int n) _ _ = Int n
subst (Fun y e') x e
  | x == y = Fun y e'
  | x /= y = Fun y $ subst e' x e
subst (Var y) x e
  | x == y = e
  | x /= y = Var y
subst (Add e1 e2) x e = Add (subst e1 x e)  (subst e2 x e)
subst (Mul e1 e2) x e = Mul (subst e1 x e)  (subst e2 x e)
subst (App e1 e2) x e = App (subst e1 x e)  (subst e2 x e)


eval :: Expr -> Env -> Value
eval (Int n) _ = INT n
eval (Var x) env
  | null vs = error $ "unbound variable " ++ x
  | otherwise = head vs
  where vs = [v|(y,v) <- env, x==y]
eval (Fun x e) env = FUN x e env
eval (Add e1 e2) env = INT (n1 + n2)
  where
    INT n1 = eval e1 env
    INT n2 = eval e2 env
eval (Mul e1 e2) env = INT (n1 * n2)
  where
    INT n1 = eval e1 env
    INT n2 = eval e2 env
eval (App e1 e2) env = eval e fenv'
  where
    FUN x e fenv = eval e env
    v2 = eval e2 env
    fenv' = (x,v2):fenv

isValue :: Expr -> Bool
isValue (Int _) = True
isValue (Fun _ _) = True
isValue _ = False

-- test
test1 = (App (Fun "x" (Add (Var "x") (Int 1))) (Var "x"))