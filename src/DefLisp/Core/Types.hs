{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Deflisp.Core.Types where

import GHC.Generics (Generic)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Hashable

import Control.Monad.Error
-- import Control.Monad.State

-- type Environment k v = H.BasicHashTable k v

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

data LispNum = Integer | Int

data LispFunk = UserFunction [LispEnvironment] [LispExpression] LispExpression |
                VarArgFunction [LispEnvironment] [LispExpression] LispExpression LispExpression |
                Macros [LispExpression] LispExpression |
                VariadicMacros [LispExpression] LispExpression LispExpression |
                LibraryFunction String ([LispExpression] -> LispExpression)
              deriving (Generic, Eq, Ord)

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
                    deriving (Generic, Eq, Ord)

-- LispFunction (LispVector [LispSymbol "a", LispSymbol "a"]) (LispNumber 1)

instance Hashable LispExpression where
  hashWithSalt s (LispSymbol n) = s `hashWithSalt`
                                  (0::Int) `hashWithSalt` n

  hashWithSalt s n = s `hashWithSalt`
                     (0::Int) `hashWithSalt` n


instance Eq ([LispExpression] -> LispExpression) where
  a == b = False

instance Ord ([LispExpression] -> LispExpression) where
  compare _ _ = EQ




mklNumber :: Integer -> LispExpression
mklNumber n = LispNumber n

mklSymbol :: String -> LispExpression
mklSymbol n = LispSymbol n

mklString :: String -> LispExpression
mklString n = LispString n

class LispLiteral l where
  toSexp :: l -> LispExpression

instance LispLiteral Integer where
  toSexp n = LispNumber n

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

mklList :: [Integer] -> LispExpression
mklList a = LispList $ map toSexp a
-- LispList [1,2,3]


-- List operations
class LispCollection l where
  lfirst :: l -> LispExpression
  next :: l -> LispExpression
  llast :: l -> LispExpression
  conj :: l -> LispExpression -> LispExpression
  cons :: l -> LispExpression -> LispExpression
  count :: l -> LispExpression
  -- cons :: l -> LispExpression -> [LispExpression]



instance LispCollection LispExpression where
  lfirst (LispList []) = LispNil -- todo add empty collection handling
  lfirst (LispList (l:_)) = l -- todo add empty collection handling
  lfirst _ = error "Can't get next of whatnot"

  next (LispList []) = LispNil
  next (LispList (_:l)) = LispList l
  next _ = error "Can't get next of whatnot"

  llast (LispList []) = LispNil
  llast (LispList l) = last l
  llast _ = error "Can't get last of whatnot"

  conj (LispList l) e = LispList $ l ++ [e]
  conj (LispList []) e = LispList $ [e]

  cons e (LispList l) = LispList $ e:l
  cons e (LispList []) = LispList $ [e]

  -- count (LispList []) = LispNumber 0
  -- count (LispList a) = LispNumber $ length a
  -- count _ = error "Can only perform count on lists and vectors"

class IsTrue l where
  isTrue :: l -> Bool

instance IsTrue LispExpression where
  isTrue LispNil = False
  isTrue (LispBool True) = True
  isTrue (LispBool False) = False
  isTrue _ = True
