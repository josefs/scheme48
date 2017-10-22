module Expr where

data LispExp = Atom String
             | List [LispExp]
             | DottedList [LispExp] LispExp
             | Number Integer
             | String String
             | Bool Bool

showExp :: LispExp -> String
showExp (String contents) = "\"" ++ contents ++ "\""
showExp (Atom name) = name
showExp (Number contents) = show contents
showExp (Bool True) = "#t"
showExp (Bool False) = "#f"
showExp (List contents) = "(" ++ unwordsList contents ++ ")"
showExp (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++
                                        showExp tail ++ ")"

unwordsList :: [LispExp] -> String
unwordsList = unwords . map showExp

instance Show LispExp where
  show = showExp
