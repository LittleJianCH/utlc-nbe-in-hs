module Normalize (

) where

import Expr
import Context
import Eval

import Data.Functor
import Control.Monad
import qualified Data.Map as M

{-
the result of evaluating an expression is almost a normalized expression,
expect the variable shadowing.
then we use normalize to solve the variable shadowing.
-}

freshen :: [String] -> String -> String
freshen used x
  | x `elem` used = freshen (x:used) (x ++ "'")
  | otherwise = x

normalize' :: [String] -> Expr -> Either String Expr
normalize' _ v@(Var _) = Right v
normalize' used l@(Lam x body) =
  let x' = freshen used x
  in doApply emptyCtx l (Var x') >>= normalize' (x':used) <&> Lam x'
normalize' used (App e1 e2) = App <$> normalize' used e1 <*> normalize' used e2
normalize' used (Let x e1 e2) = Left "There won't be any let in evaled expression"
normalize' _ Zero = Right Zero
normalize' used (Succ e) = Succ <$> normalize' used e
normalize' used (Rec n start iter) =
  Rec <$> normalize' used n <*> normalize' used start <*> normalize' used iter
normalize' used (Cons e1 e2) = Cons <$> normalize' used e1 <*> normalize' used e2
normalize' used (Car e) = Car <$> normalize' used e
normalize' used (Cdr e) = Cdr <$> normalize' used e

normalize :: Expr -> Either String Expr
normalize = eval emptyCtx >=> normalize' []

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