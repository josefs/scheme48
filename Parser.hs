module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Eval

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left  err -> throwError $ Parser err
    Right exp -> return exp

readExpr :: String -> ThrowsError LispExp
readExpr = readOrThrow parseExpr
readExprList :: String -> ThrowsError [LispExp]
readExprList = readOrThrow (endBy parseExpr spaces)

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
