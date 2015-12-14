module Parser where

import Types
import Text.ParserCombinators.Parsec
import Control.Monad

programToAST :: Program -> Either ParseError MyExpr
programToAST = parse parseExpr ""

parseExpr :: Parser MyExpr
parseExpr = try parseString <|>
            try parseInt <|>
            try parseFunction <|>
            try parseApp

parseFunction :: Parser MyExpr
parseFunction = string "keith" >> return (MyInt 3)

parseInt :: Parser MyExpr
parseInt = liftM (MyInt . read) $
  lexeme (string "garrett") *> lexeme (many1 digit) 

parseString :: Parser MyExpr
parseString = liftM MyString $
  lexeme (string "eugene") *> char '"' 
  *> lexeme (many1 alphaNum) <* 
  char '"'

parseApp :: Parser MyExpr
parseApp  = do
  symbol <- many (letter)
  x <- parseExpr
  y <- parseExpr
  return $ MyApplication (MySymbol symbol) [x, y]

ws :: Parser String
ws = many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme ps = ws *> ps <* ws
