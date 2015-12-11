module Evaluator where

import Types

evaluate :: Expr -> Expr
evaluate x@(MyInt n) = x
evaluate x@(MyString str) = x
evaluate (MyAdd (MyInt x) (MyInt y)) = MyInt $ x + y
evaluate (MyAdd (MyString x) (MyString y)) = MyString $ x ++ y
