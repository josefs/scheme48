module Eval where

import Expr

eval :: LispExp -> LispExp
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispExp] -> LispExp
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispExp] -> LispExp)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispExp] -> LispExp
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispExp -> Integer
unpackNum (Number n) = n
-- This stuff is highly dubious!
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
