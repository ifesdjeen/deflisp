module Deflisp.Core.Show where

import Deflisp.Core.Types


instance Show LispFunk where
  show (LibraryFunction name _) = "library function: " ++ name
  show (VarArgFunction _ _ _ _) = "vararg function"
  show (UserFunction _ _ _) = "user function"
  show (Macros _ _) = "macros"
  show (VariadicMacros _ _ _) = "variadic macros"

instance Show LispExpression where
  show (LispNumber n) = show n
  show (LispSymbol n) = n
  show (ReservedKeyword n) = show n
  show (LispList n) = "(" ++ (unwords (map show n)) ++ ")"
  show (LispVector n) = "[" ++ (unwords (map show n)) ++ "]"
  show (LispString n) = n
  show (LispBool n) = show n
  show (LispFunction a) = show a
  show (LispNil) = "nil"

-- instance Show (IO LispExpression) where
--   show (IO (LispNumber n)) = show n

-- instance Eq LispExpression where
  -- (LispNumber a) == (LispNumber b)  = a == b
  -- (LispBool a) == (LispBool b)  = a == b
  -- (LispSymbol a) == (LispSymbol b)  = a == b
  -- (LispList a) == (LispList b)  = a == b
  -- (LispVector a) == (LispVector b)  = a == b
  -- (LispString a) == (LispString b)  = a == b
  -- LispNil == LispNil  = True
  -- _ == _ = False
