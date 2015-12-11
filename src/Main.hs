module Main where

import Types
import Parser
import Evaluator

main = runProgram "STRING \"hi\" + STRING \"aonetuh\""

runProgram :: Program -> IO ()
runProgram = print . fmap evaluate . programToAST
