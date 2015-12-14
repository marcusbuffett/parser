module Main where

import Types
import Parser
import Evaluator
import Control.Monad.State.Lazy

main = do
   runAST [(MyVar "x" (MyInt 4)), MyVar "thingy" (MyApplication (MySymbol "plus") [MyInt 3, MySymbol "x", MyInt 3])]
   {- runProgram "INT 3 + INT 234" -}

{- runProgram :: Program -> IO () -}
{- runProgram = print . fmap ((flip evalStateT) initialContext . evaluate) . programToAST -}

runAST :: [MyExpr] -> IO ()
runAST ast = print $ foldM (\ctx expr -> liftM snd (runStateT (evaluate expr) ctx)) [initialContext] ast
