module Types where

data Expr = MyInt Int | 
            MyString String |
            MyAdd Expr Expr |
            MyPrint Expr
            deriving (Show, Eq)

type Program = String
