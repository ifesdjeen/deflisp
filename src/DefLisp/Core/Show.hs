module DefLisp.Core.Show where

import DefLisp.Core.Types

unwordsList :: [LispExpression] -> String
unwordsList = unwords . map showVal -- ()

showVal :: LispExpression -> String
showVal (LispNumber x) = show x
showVal (LispSymbol x) = "\'" ++ x
showVal (LispList x) = "(" ++ unwordsList x ++ ")"
showVal (LispString x) = "\"" ++ x ++ "\""
showVal (LispBool _) = ""
