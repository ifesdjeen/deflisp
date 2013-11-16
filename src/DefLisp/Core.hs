module DefLisp.Core where

import DefLisp.Core.Types
import DefLisp.Core.Show
import DefLisp.Core.Parser

import Text.ParserCombinators.Parsec

import Data.IORef
import Control.Monad.Error
-- import Debug.Trace

import qualified Data.HashTable.IO as H

type IOThrowsError = ErrorT LispError IO

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

type Environment k v = H.BasicHashTable k v

type LispEnvironment = (Environment LispExpression LispExpression)

freshEnv :: IO LispEnvironment
freshEnv = do
   env <- H.new
   return env

defineVar :: LispEnvironment -> LispExpression -> LispExpression -> IO (LispExpression)
defineVar env symbol expr = do
  H.insert env symbol expr
  return symbol

findVar :: Environment LispExpression LispExpression -> LispExpression -> IO (Maybe LispExpression)
findVar env symbol = do
  res <- H.lookup env symbol
  return res

-- bar :: IO (HashTable Int Int) ->

unpackNum :: LispExpression -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispList [n]) = unpackNum n
unpackNum (LispString s) = read s :: Integer

--
-- Numerical Operations
--

numericOp :: (Integer -> Integer -> Integer) -> [LispExpression] -> LispExpression
numericOp op args = LispNumber $ foldl1 op $ map unpackNum args

builtInOp :: String -> [LispExpression] -> IO LispExpression
-- builtInOp "+" args | trace(show $ foldl1 (+) $ map unpackNum args) False = undefined
builtInOp "+" args = return $ numericOp (+) args
builtInOp "-" args = return $ numericOp (-) args
builtInOp "*" args = return $ numericOp (*) args
builtInOp "/" args = return $ numericOp (div) args

eval :: LispEnvironment -> LispExpression -> IO LispExpression
eval _ val@(LispString _) = return val
eval _ val@(LispSymbol _) = return val

eval envRef (LispList[LispSymbol "def", LispSymbol var, form]) =
-- eval envRef form >>= defineVar envRef var
  eval envRef form >>= defineVar envRef (toSexp var)
-- defineVar envRef var form

-- eval env (List [Atom "define", Atom var, form]) =
--     eval env form >>= defineVar env var

eval envRef (LispList (LispSymbol func: args)) =
  mapM (eval envRef) args >>= builtInOp func

eval _ val@(LispNumber _) = return val
eval _ val@(LispBool _) = return val

-- evalExpression :: String -> String
-- (eval . readExpression) "(+ 1 1)"

t :: String -> IO LispEnvironment
t exp = do
  -- ht <- freshEnv
  ht <- H.new
  let sym = mklSymbol "sym"
      num = mklNumber 1
      read = readExpression exp
      evaled = eval ht read
  --defineVar ht sym num
  --lookedup <- findVar ht sym
  --print $ (fromJust lookedup)
  s <- liftM show evaled
  lookedup <- findVar ht sym
  print $ (fromJust lookedup)
  print s


  return ht
