module Normalize (
  normalize,
  exprEqual,
) where

import Expr ( Expr(..) )
import Context ( emptyCtx )
import Eval ( eval )

import qualified Data.Map as M

normalize :: Expr -> Either String Expr
normalize = eval emptyCtx []

alphaEquiv :: M.Map String String -> Expr -> Expr -> Bool
alphaEquiv env (Var x) (Var y) = M.lookup x env == Just y
alphaEquiv env (Lam x e1) (Lam y e2) = alphaEquiv (M.insert x y env) e1 e2
alphaEquiv env (App e1 e2) (App e3 e4) = alphaEquiv env e1 e3 && alphaEquiv env e2 e4
alphaEquiv env Zero Zero = True
alphaEquiv env (Succ e1) (Succ e2) = alphaEquiv env e1 e2
alphaEquiv env (Rec n start iter) (Rec n' start' iter') =
  alphaEquiv env n n' && alphaEquiv env start start' && alphaEquiv env iter iter'
alphaEquiv env (Cons e1 e2) (Cons e3 e4) = alphaEquiv env e1 e3 && alphaEquiv env e2 e4
alphaEquiv env (Car e1) (Car e2) = alphaEquiv env e1 e2
alphaEquiv env (Cdr e1) (Cdr e2) = alphaEquiv env e1 e2
alphaEquiv _ _ _ = False

exprEqual :: Expr -> Expr -> Either String Bool
exprEqual e1 e2 = alphaEquiv M.empty <$> normalize e1 <*> normalize e2