module Context (
  Context,
  lookupCtx,
  extendCtx,
  emptyCtx,
) where

import Expr ( Expr )

import qualified Data.Map as Map

type Context = Map.Map String Expr

lookupCtx :: String -> Context -> Maybe Expr
lookupCtx = Map.lookup

extendCtx :: String -> Expr -> Context -> Context
extendCtx = Map.insert

emptyCtx :: Context
emptyCtx = Map.empty