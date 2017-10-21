module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Expr

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right exp -> "Found "     ++ showExp exp

----------------------------------------
-- Parsing expressions
----------------------------------------

parseString :: Parser LispExp
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispExp
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispExp
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispExp
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispExp
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispExp
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispExp
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
