{-# LANGUAGE LambdaCase #-}

module Eval (
  eval,
  doApply,
) where

import Context ( Context, lookupCtx, extendCtx, ctxKeys )
import Expr ( Expr(..) )

import Data.Functor ( (<&>) )
import Data.Either.Utils ( maybeToEither )
import Control.Monad ( join, liftM2, liftM3 )
import Control.Monad.Trans.Reader ( ReaderT, ask, local )

type EvalS = ReaderT Context (Either String)

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

getContext :: EvalS Context
getContext = ask

getUsed :: EvalS [String]
getUsed = getContext <&> ctxKeys

freshen :: String -> EvalS String
freshen x = getUsed >>= elem x >.> (\case
  True -> freshen $ x ++ "'"
  False -> return x)

lookupVar :: String -> EvalS Expr
lookupVar x = getContext >>= maybeToEither ("Undefined variable " ++ show x) . lookupCtx x

eval :: Expr -> EvalS Expr
eval (Var x) = lookupVar x
eval lam@(Lam x _) = do
  x' <- freshen x
  Lam x' <$> local (extendCtx x' (Var x')) (doApply lam (Var x'))
eval (App e1 e2) = join $ liftM2 doApply (eval e1) (eval e2)
eval (Let x e1 e2) = eval (App (Lam x e1) e2)
eval Zero = return Zero
eval (Succ e) = Succ <$> eval e
eval (Rec n start step) = join $ liftM3 doRec (eval n) (eval start) (eval step)
eval (Cons e1 e2) = Cons <$> eval e1 <*> eval e2
eval (Car e) = eval e >>= \case
  Cons car _ -> return car
  e' -> return $ Car e'
eval (Cdr e) = eval e >>= \case
  Cons _ cdr -> return cdr
  e' -> return $ Cdr e'

doApply :: Expr -> Expr -> EvalS Expr
doApply (Lam x body) e2 = local (extendCtx x e2) $ eval body
doApply e1 e2 = return $ App e1 e2

doRec :: Expr -> Expr -> Expr -> EvalS Expr
doRec Zero start _ = return start
doRec n@(Succ n') start step
  = join $ doApply <$> doApply step n
                   <*> doRec n' start step
doRec n start step = return $ Rec n start step