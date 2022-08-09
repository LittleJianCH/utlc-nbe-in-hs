module Main ( main ) where

import Expr
import Context
import Normalize

import Test.HUnit
import System.Exit

testId :: Test
testId =
  let id1 = Lam "x" (Var "x")
      id2 = Lam "y" (Var "y")
      id3 = App id1 id2
      id4 = Lam "x" (Car (Cons (Var "x") (Var "x")))
  in TestList [
    TestCase $ assertEqual "Test Id-1" (Right True) (exprEqual id1 id1)
  , TestCase $ assertEqual "Test Id-2" (Right True) (exprEqual id1 id2)
  , TestCase $ assertEqual "Test Id-3" (Right True) (exprEqual id1 id3)
  , TestCase $ assertEqual "Test Id-4" (Right True) (exprEqual id1 id4)
  ]

num2Nat :: Int -> Expr
num2Nat 0 = Zero
num2Nat n = Succ (num2Nat (n - 1))

testAdd :: Test
testAdd =
  let succ_ = Lam "n" (Lam "x" (Succ (Var "x")))
      add = Lam "x" (Lam "y" (Rec (Var "x") (Var "y") succ_))
  in TestList [TestCase (assertEqual (concat ["Test Add ", show x, " ", show y]) 
                                     (Right True) 
                                     (exprEqual (App (App add (num2Nat x)) (num2Nat y)) 
                                                (num2Nat $ x + y)))
              | x <- [1 .. 20], y <- [1 .. 20]]

testFlip :: Test
testFlip = 
  let flip :: Expr -> Expr
      flip f = Lam "a" (Lam "b" (App (App f (Var "b")) (Var "a")))
      f1 = Lam "f" (Var "f")
      f2 = flip f1
      f3 = flip $ flip f1
      f4 = flip $ flip $ flip $ flip f1
      f5 = Lam "x" (Lam "y" (App (App f1 (Var "x")) (Var "y")))
  in TestList [
    TestCase $ assertEqual "Test flip-1-1" (Right True) (exprEqual f1 f1)
  , TestCase $ assertEqual "Test flip-1-2" (Right False) (exprEqual f1 f2)
  , TestCase $ assertEqual "Test flip-2-3" (Right False) (exprEqual f2 f3)
  , TestCase $ assertEqual "Test flip-1-3" (Right False) (exprEqual f1 f3)
  , TestCase $ assertEqual "Test flip-3-4" (Right True) (exprEqual f3 f4)
  , TestCase $ assertEqual "Test flip-3-5" (Right True) (exprEqual f3 f5)
  ]

testGuass :: Test
testGuass =
  let succ_ = Lam "n" (Lam "x" (Succ (Var "x")))
      add = Lam "x" (Lam "y" (Rec (Var "x") (Var "y") succ_))

      guassExpr :: Expr -> Expr
      guassExpr n = Rec n Zero add

      guassInt :: Int -> Int
      guassInt n = n * (n + 1) `div` 2

  in TestList [TestCase $ assertEqual ("Test Guass-" ++ show n) 
                                      (Right True) 
                                      (exprEqual (num2Nat $ guassInt n) 
                                                 (guassExpr $ num2Nat n))
              | n <- [1 .. 30]]

testRename :: Test
testRename =
  let expr = Lam "x" (Lam "x" (Lam "x'" (Cons (Var "x") (Var "x'"))))
  in TestCase $ assertEqual "Test Rename" 
                            (Right $ Lam "x" (Lam "x'" (Lam "x''" (Cons (Var "x'")
                                                                        (Var "x''")))))
                            (normalize expr)

main :: IO ()
main = do
  results <- runTestTT $ TestList [testId, testAdd, testFlip, testGuass, testRename]
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)