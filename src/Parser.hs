module Parser where

import Types
import Text.ParserCombinators.Parsec
import Control.Monad

programToAST :: Program -> Either ParseError Expr
programToAST = parse programParser ""

programParser :: Parser Expr
programParser = try parseAdd <|> try parseInt <|> try parseString

parseInt :: Parser Expr
parseInt = liftM (MyInt . read) $
  lexeme (string "INT") *> lexeme (many1 digit) 

parseString :: Parser Expr
parseString = liftM MyString $
  lexeme (string "STRING") *> char '"' 
  *> lexeme (many1 alphaNum) <* 
  char '"'

parseAdd :: Parser Expr
parseAdd = do
  x <- try parseInt <|> parseString
  lexeme (char '+')
  y <- try parseInt <|> parseString
  return $ MyAdd x y

ws :: Parser String
ws = many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme ps = ws *> ps <* ws
