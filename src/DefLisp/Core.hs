module DefLisp.Core where

import DefLisp.Core.Types
import DefLisp.Core.Show
import DefLisp.Core.Parser

-- import Debug.Trace

import Text.ParserCombinators.Parsec

unpackNum :: LispExpression -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispString s) = read s :: Integer

--
-- Numerical Operations
--

numericOp :: (Integer -> Integer -> Integer) -> [LispExpression] -> LispExpression
numericOp op args = LispNumber $ foldl1 op $ map unpackNum args

builtInOp :: String -> [LispExpression] -> LispExpression
-- builtInOp "+" args | trace(show $ foldl1 (+) $ map unpackNum args) False = undefined
builtInOp "+" args = numericOp (+) args
builtInOp "-" args = numericOp (-) args
builtInOp "*" args = numericOp (*) args
builtInOp "/" args = numericOp (div) args

eval :: LispExpression -> LispExpression
eval val@(LispString _) = val
eval val@(LispSymbol _) = val
-- eval val@(LispList _) = val
-- eval (LispList (LispSymbol _: args)) = LispNumber $ foldl1 (+) $ map unpackNum args
eval (LispList (LispSymbol func: args)) = builtInOp func args
eval val@(LispNumber _) = val
eval val@(LispBool _) = val


-- evalExpression :: String -> String

-- (eval . readExpression) "(+ 1 1)"
