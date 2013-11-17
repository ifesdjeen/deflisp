{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module DefLisp.Core.Types where

import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Hashable

import Control.Monad.Error
-- import Control.Monad.State



type SymbolTable = Map.Map String LispExpression
data Context = Ctx SymbolTable (Maybe Context)

type ThrowsError = Either LispError

data LispError = NumArgs Integer [LispExpression]
               | TypeMismatch String LispExpression
               | UnboundVar String String
               | Default String

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

-- type LispResult = StateT Context LispError LispExpression

data ReservedKeyword = DefKeyword |
                       IfKeyword
                     deriving (Show, Eq)

data LispExpression = LispSymbol String |
                      ReservedKeyword ReservedKeyword |
                      LispList [LispExpression] |
                      LispNumber Integer |
                      LispString String |
                      LispBool Bool
                      deriving (Show, Eq, Generic)

instance Hashable LispExpression where
  hashWithSalt s (LispSymbol n) = s `hashWithSalt`
                                  (0::Int) `hashWithSalt` n

  hashWithSalt s (LispNumber n) = s `hashWithSalt`
                                  (0::Int) `hashWithSalt` n

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
  toSexp n = LispSymbol n

instance LispLiteral Bool where
  toSexp n = LispBool n

instance LispLiteral [Integer] where
  toSexp n = LispList $ map toSexp n

mklList :: [Integer] -> LispExpression
mklList a = LispList $ map toSexp a
-- LispList [1,2,3]
