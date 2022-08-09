{-# LANGUAGE LambdaCase #-}

module Eval (
  eval,
  doApply,
) where

import Context ( Context, lookupCtx, extendCtx )
import Expr ( Expr(..) )

import Data.Functor ( (<&>) )
import Data.Either.Utils ( maybeToEither )
import Control.Monad ( join )

freshen :: [String] -> String -> String
freshen used x
  | x `elem` used = freshen (x:used) (x ++ "'")
  | otherwise = x

eval :: Context -> [String] -> Expr -> Either String Expr
eval ctx _ (Var x) = maybeToEither ("Variable " ++ x ++ " is unbounded.") $ lookupCtx x ctx
eval ctx used l@(Lam x body) =
  let x' = freshen used x
  in doApply (extendCtx x' (Var x') ctx) (x':used) l (Var x') <&> Lam x'
eval ctx used (App e1 e2) = join $ doApply ctx used <$> eval ctx used e1 
                                                    <*> eval ctx used e2
eval ctx used (Let x e1 e2) = eval ctx used (App (Lam x e1) e2)
eval ctx _ Zero = Right Zero
eval ctx used (Succ n) = eval ctx used n <&> Succ
eval ctx used (Rec n start step) 
  = join $ doRec ctx used <$> eval ctx used n
                          <*> eval ctx used start 
                          <*> eval ctx used step
eval ctx used (Cons e1 e2) = Cons <$> eval ctx used e1 <*> eval ctx used e2
eval ctx used (Car e) = eval ctx used e <&> \case
  Cons e1 _ -> e1
  expr -> Car expr
eval ctx used (Cdr e) = eval ctx used e <&> \case
  Cons _ e2 -> e2
  expr -> Cdr expr

doApply :: Context -> [String] -> Expr -> Expr -> Either String Expr
doApply ctx used (Lam x body) e2 = eval (extendCtx x e2 ctx) used body
doApply _ _ e1 e2 = Right $ App e1 e2

doRec :: Context -> [String] -> Expr -> Expr -> Expr -> Either String Expr
doRec _ _ Zero start _ = Right start
doRec ctx used n@(Succ n') start step 
  = join $ doApply ctx used <$> doApply ctx used step n 
                            <*> doRec ctx used n' start step
doRec _ _ n start step = Right $ Rec n start step