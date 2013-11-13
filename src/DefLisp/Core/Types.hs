module DefLisp.Core.Types where

import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State



type SymbolTable = Map.Map String LispExpression
data Context = Ctx SymbolTable (Maybe Context)



type LispError = ErrorT String IO
type LispResult = StateT Context LispError LispExpression


data LispExpression = LispSymbol String |
                      LispList [LispExpression] |
                      LispNumber Integer |
                      LispString String |
                      LispBool Bool
                      deriving (Show)
