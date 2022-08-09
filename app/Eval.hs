{-# LANGUAGE LambdaCase #-}

module Eval (
  eval,
) where

import Context ( Context, lookupCtx, extendCtx )
import Expr ( Expr(..) )

import Data.Functor ( (<&>) )
import Data.Either.Utils ( maybeToEither )
import Control.Monad ( join )

eval :: Context -> Expr -> Either String Expr
eval ctx (Var x) = maybeToEither ("Variable" ++ x ++ "is unbounded.") $ lookupCtx x ctx
eval ctx (Lam x body) = eval (extendCtx x (Var x) ctx) body <&> Lam x
eval ctx (App e1 e2) = join $ doApply ctx <$> eval ctx e1 <*> eval ctx e2
eval ctx (Let x e1 e2) = eval ctx (App (Lam x e1) e2)
eval ctx Zero = Right Zero
eval ctx (Succ n) = eval ctx n <&> Succ
eval ctx (Rec n start step) 
  = join $ doRec ctx <$> eval ctx n <*> eval ctx start <*> eval ctx step
eval ctx (Cons e1 e2) = Cons <$> eval ctx e1 <*> eval ctx e2
eval ctx (Car e) = eval ctx e <&> \case
  Cons e1 _ -> e1
  expr -> Car expr
eval ctx (Cdr e) = eval ctx e <&> \case
  Cons _ e2 -> e2
  expr -> Cdr expr

doApply :: Context -> Expr -> Expr -> Either String Expr
doApply ctx (Lam x body) e2 = eval (extendCtx x e2 ctx) body
doApply _ e1 e2 = Right $ App e1 e2

doRec :: Context -> Expr -> Expr -> Expr -> Either String Expr
doRec _ Zero start _ = Right start
doRec ctx n@(Succ n') start step 
  = join $ doApply ctx <$> doApply ctx step n <*> doRec ctx n start step
doRec ctx n start step = Right $ Rec n start step