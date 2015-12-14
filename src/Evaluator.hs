module Evaluator where

import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Types
import Control.Monad

evaluate :: MyExpr -> State [Context] MyExpr
evaluate x@(MyInt n) = return x
evaluate x@(MyString str) = return x
evaluate x@(MyFunction args lines) = return x
evaluate x@(MyBinaryOp f) = return x
evaluate (MySymbol str) = do
  ctxs <- get 
  return $ getVar str ctxs
evaluate (MyVar str expr) = do
  ctxs <- get
  e <- evaluate expr
  let newCtxs = updateContexts str e ctxs 
  put newCtxs
  return e
  {- evaluate expr >>= \e -> modify (M.insert str e . last) >> return e -}
  where 
evaluate (MyApplication (MyBinaryOp f) es) = do
  params <- mapM evaluate es
  return $ f (params !! 0) (params !! 1)
evaluate (MyApplication e es) = do
  params <- mapM evaluate es
  (MyFunction args lines) <- evaluate e
  mapM_ (\(arg, param) -> modify (\ctxs -> (updateContexts arg param ctxs))) $
    zip args params
  {- f args -}
  {- undefined -}
  liftM head $ mapM evaluate lines
  {- return $ f args -}
{- evaluate (MyAdd (MyInt x) (MyInt y)) = return $ MyInt $ x + y -}

updateContexts str expr ctxs = do
  let lastCtx = last ctxs
  let rest = init ctxs
  rest ++ [M.insert str expr lastCtx]

getVar :: String -> [Context] -> MyExpr
getVar str ctxs = do
  fromJust $ foldl (\result ctx -> if isJust result then result else M.lookup str ctx) Nothing ctxs

initialContext :: Context
initialContext = M.fromList [
                ("plus", MyFunction ["x", "y"] [MyApplication (MyBinaryOp myAdd) [MySymbol "x", MySymbol "y"]]),
                ("life", MyInt 42)
                ]
  where
    myAdd :: MyExpr -> MyExpr -> MyExpr
    myAdd (MyInt x) (MyInt y) = MyInt (x + y)
