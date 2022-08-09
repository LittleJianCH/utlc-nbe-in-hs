module Expr( 
  Expr(..),
) where

data Expr = Var String
          | Lam String Expr
          | App Expr Expr
          | Let String Expr Expr
          | Zero 
          | Succ Expr
          | Rec Expr Expr Expr
          | Cons Expr Expr
          | Car Expr
          | Cdr Expr
  deriving (Eq, Show)