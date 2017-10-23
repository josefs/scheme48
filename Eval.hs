{-# LANGUAGE ExistentialQuantification #-}
module Eval where

import Expr
import Control.Monad.Except
import Data.Functor.Identity
import Text.ParserCombinators.Parsec (ParseError)
import Data.IORef

----------------------------------------
-- Environment
----------------------------------------

-- This looks higly dubious. Why the outer IORef?
type Env = IORef [(String, IORef LispExp)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispExp
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispExp -> IOThrowsError LispExp
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispExp -> IOThrowsError LispExp
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispExp)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

----------------------------------------
-- Evaluator
----------------------------------------

eval :: Env -> LispExp -> IOThrowsError LispExp
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id)      = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
          Bool True  -> eval env conseq
          Bool False -> eval env alt
          otherwise  -> throwError $ TypeMismatch "bool" result
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom func : args)) = liftThrows . apply func =<< mapM (eval env) args
eval env badform = throwError $ BadSpecialForm "Unrecognized special form" badform

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
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispExp] -> ThrowsError LispExp
numericBinop op []     = throwError $ NumArgs 2 []
numericBinop op s@[_]  = throwError $ NumArgs 2 s
numericBinop op params = return . Number . foldl1 op
                     =<< mapM unpackNum params

boolBinop :: (LispExp -> ThrowsError a) -> (a -> a -> Bool) ->
             [LispExp] -> ThrowsError LispExp
boolBinop unpacker op [arg0,arg1] = do
  left  <- unpacker arg0
  right <- unpacker arg1
  return $ Bool $ left `op` right
boolBinop unpacker op args = throwError $ NumArgs 2 args

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispExp -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispExp -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispExp -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] in
  if null parsed
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

car :: [LispExp] -> ThrowsError LispExp
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispExp] -> ThrowsError LispExp
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispExp] -> ThrowsError LispExp
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispExp] -> ThrowsError LispExp
eqv [(Bool arg1), (Bool arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]     = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]     = return $ Bool $
  (length arg1 == length arg2) &&
  (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) =
             -- FIXME : I need to do something better here.
             case eqv [x1, x2] of
               ExceptT (Identity (Left err)) -> False
               ExceptT (Identity (Right (Bool val))) -> val
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispExp -> ThrowsError a)

unpackEquals :: LispExp -> LispExp -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

-- This is a pretty clumsy implementation. It'd be better to
-- first unpack the first argument and based on that see how we should
-- unpack the seoncd argument.
equal :: [LispExp] -> ThrowsError LispExp
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum
                         ,AnyUnpacker unpackStr
                         ,AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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
{-
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default
-}
type IOThrowsError = ExceptT LispError IO

type ThrowsError = Except LispError

trapError action = catchError action (return . show)

-- Awful! I should get rid of this function altogether
extractValue :: Either b a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
  runExceptT (trapError action) >>= return . extractValue

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (ExceptT (Identity (Left  err))) = throwError err
liftThrows (ExceptT (Identity (Right val))) = return val
