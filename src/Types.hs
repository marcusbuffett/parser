module Types where

import Control.Monad.ST.Strict
import Data.Map

data MyExpr = MyInt Int | 
            MyString String |
            MyPrint MyExpr |
            MyFunction [String] [MyExpr] |
            MyBuiltin ([MyExpr] -> MyExpr) |
            MyVar String MyExpr |
            MySymbol String |
            MyBinaryOp (MyExpr -> MyExpr -> MyExpr) |
            MyUnaryOp (MyExpr -> MyExpr) |
            MyApplication MyExpr [MyExpr]

instance Show MyExpr where
  show (MyFunction args lines)         = "Function"
  show (MyInt x)               = "Integer " ++ show x
  show (MyString x)            = "String " ++ x
  show (MyPrint x)             = "Print " ++ show x
  show (MyVar str expr)        = "Var " ++ str ++ show expr
  show (MySymbol str)          = "Symbol " ++ str
  show (MyBuiltin f)           = "Built in! "
  show (MyBinaryOp f)          = "Binary op"
  show (MyApplication f xs)    = "Application " ++ show f ++ show xs

type Program = String
type Context = Map String MyExpr
