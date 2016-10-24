{-# OPTIONS_GHC -Wall #-}

module Homework where

import ExprT
import Parser (parseExp)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

instance Expr Integer where
  lit n = n
  add n1 n2 = n1 + n2
  mul n1 n2 = n1 * n2

instance Expr Bool where
  lit n
    | n <= 0    = False
    | otherwise = True
  add b1 b2 = b1 || b2
  mul b1 b2 = b1 && b2

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax n1) (MinMax n2) = MinMax (max n1 n2)
  mul (MinMax n1) (MinMax n2) = MinMax (min n1 n2)

instance Expr Mod7 where
  lit n = Mod7 n
  add (Mod7 n1) (Mod7 n2) = Mod7 ((n1 + n2) `mod` 7)
  mul (Mod7 n1) (Mod7 n2) = Mod7 ((n1 * n2) `mod` 7)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr str =
  case parseExp Lit Add Mul str of
    Just(e) -> Just(eval e)
    Nothing -> Nothing

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
