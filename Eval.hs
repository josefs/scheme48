module Eval where

import Expr
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)


eval :: LispExp -> ThrowsError LispExp
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> [LispExp] -> ThrowsError LispExp
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispExp] -> ThrowsError LispExp)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispExp] -> ThrowsError LispExp
numericBinop op []     = throwError $ NumArgs 2 []
numericBinop op s@[_]  = throwError $ NumArgs 2 s
numericBinop op params = return . Number . foldl1 op
                     =<< mapM unpackNum params

unpackNum :: LispExp -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] in 
  if null parsed 
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

----------------------------------------
-- Error handling
----------------------------------------

data LispError = NumArgs Integer [LispExp]
               | TypeMismatch String LispExp
               | Parser ParseError
               | BadSpecialForm String LispExp
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
