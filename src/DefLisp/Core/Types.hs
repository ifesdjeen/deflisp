{-# LANGUAGE FlexibleInstances #-}

module Deflisp.Core.Types where

import Control.Monad.State
import qualified Data.Map as Map


import Control.Monad.Error

type LispEnvironment = (Map.Map LispExpression LispExpression)
type Context = State LispEnvironment LispExpression

type ThrowsError = Either LispError

data LispError = NumArgs Integer [LispExpression]
               | TypeMismatch String LispExpression
               | UnboundVar String String
               | Default String

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

data ReservedKeyword = DefKeyword |
                       FnKeyword |
                       NilKeyword |
                       DefMacroKeyword |
                       IfKeyword
                     deriving (Show, Eq, Ord)

data LispFunk = UserFunction [LispEnvironment] [LispExpression] LispExpression |
                VarArgFunction [LispEnvironment] [LispExpression] LispExpression LispExpression |
                Macros [LispExpression] LispExpression |
                VariadicMacros [LispExpression] LispExpression LispExpression |
                LibraryFunction String ([LispExpression] -> LispExpression)
              deriving (Eq, Ord)

data LispExpression = LispSymbol String |
                      ReservedKeyword ReservedKeyword |
                      LispList [LispExpression] |
                      LispVector [LispExpression] |
                      LispNumber Integer |
                      --LispNumber LispNum |
                      LispString String |
                      LispBool Bool |
                      LispFunction LispFunk |
                      -- LispFunction [LispExpression] LispExpression |
                      LispNil
                    deriving (Eq, Ord)

-- LispFunction (LispVector [LispSymbol "a", LispSymbol "a"]) (LispNumber 1)

instance Eq ([LispExpression] -> LispExpression) where
  _ == _ = False

instance Ord ([LispExpression] -> LispExpression) where
  compare _ _ = EQ




class LispLiteral l where
  toSexp :: l -> LispExpression

instance LispLiteral [Char] where
  toSexp "def" = ReservedKeyword DefKeyword
  toSexp "fn" = ReservedKeyword FnKeyword
  toSexp "if" = ReservedKeyword IfKeyword
  toSexp "defmacro" = ReservedKeyword DefMacroKeyword
  toSexp "nil" = LispNil
  toSexp n = LispSymbol n

-- instance LispLiteral a => LispLiteral [a] where
--   toSexp x = LispList $ map toSexp x

instance LispLiteral Bool where
  toSexp n = LispBool n

-- List operations
class LispCollection l where
  lfirst :: l -> LispExpression
  next :: l -> LispExpression
  llast :: l -> LispExpression
  conj :: l -> LispExpression -> LispExpression
  cons :: l -> LispExpression -> LispExpression
  count :: l -> LispExpression


instance LispCollection LispExpression where
  lfirst (LispList []) = LispNil -- todo add empty collection handling
  lfirst (LispList (l:_)) = l -- todo add empty collection handling

  lfirst (LispVector []) = LispNil -- todo add empty collection handling
  lfirst (LispVector (l:_)) = l -- todo add empty collection handling

  lfirst _ = error "Can't get next of whatnot"

  next (LispList []) = LispNil
  next (LispList (_:l)) = LispList l

  next (LispVector []) = LispNil
  next (LispVector (_:l)) = LispVector l

  next _ = error "Can't get next of whatnot"

  llast (LispList []) = LispNil
  llast (LispList l) = last l

  llast (LispVector []) = LispNil
  llast (LispVector l) = last l
  llast _ = error "Can't get last of whatnot"

  conj (LispList []) e = LispList $ [e]
  conj (LispList l) e = LispList $ l ++ [e]

  conj (LispVector []) e = LispList $ [e]
  conj (LispVector l) e = LispList $ l ++ [e]

  conj _ _ = error "Can't conj"

  cons e (LispList []) = LispList $ [e]
  cons e (LispList l) = LispList $ e:l

  cons e (LispVector []) = LispList $ [e]
  cons e (LispVector l) = LispList $ e:l

  cons a b = error "Can't cons: " -- ++ (show a) ++ " and " ++ (show b)

  count (LispList []) = LispNumber 0
  count (LispList a) = LispNumber $ toInteger $ length a

  count (LispVector []) = LispNumber 0
  count (LispVector a) = LispNumber $ toInteger $ length a
  count _ = error "Can only perform count on lists and vectors"

class IsTrue l where
  isTrue :: l -> Bool

instance IsTrue LispExpression where
  isTrue LispNil = False
  isTrue (LispBool True) = True
  isTrue (LispBool False) = False
  isTrue _ = True
