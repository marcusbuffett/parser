module Types where

data Expr = MyInt Int | 
            MyString String |
            MyFunc Expr Expr
            deriving (Show, Eq)
