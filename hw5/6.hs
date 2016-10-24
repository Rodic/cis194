{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT =
      Lit Integer
    | Add VarExprT VarExprT
    | Mul VarExprT VarExprT
    | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x   = \_ -> Just x
  add f g = \m -> case (f m, g m) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just x, Just y) -> Just (x + y)
  mul f g = \m -> case (f m, g m) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just x, Just y) -> Just (x * y)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
