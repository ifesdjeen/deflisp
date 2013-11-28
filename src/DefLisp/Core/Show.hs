module Deflisp.Core.Show where

import Deflisp.Core.Types


instance Show LispExpression where
  show (LispNumber n) = show n
  show (LispSymbol n) = n
  show (ReservedKeyword n) = show n
  show (LispList n) = "(" ++ (unwords (map show n)) ++ ")"
  show (LispVector n) = "(" ++ (unwords (map show n)) ++ ")"
  show (LispString n) = n
  show (LispBool n) = show n
  show (LispFunction bindings expr) = "(fn [" ++ (show bindings) ++ "] " ++ (show expr) ++ ")"
  show (LispNil) = "nil"
