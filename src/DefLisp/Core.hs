module DefLisp.Core where

import DefLisp.Core.Types
import DefLisp.Core.Show
import DefLisp.Core.Parser

import Text.ParserCombinators.Parsec

import System.IO
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
freshEnv = H.new

defineVar :: LispEnvironment -> LispExpression -> LispExpression -> IO (LispExpression)
defineVar env symbol expr = do
  H.insert env symbol expr
  return symbol

findVar :: LispEnvironment -> LispExpression -> IO (Maybe LispExpression)
findVar env symbol = H.lookup env symbol

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

liftVarToIO :: LispExpression -> IO LispExpression
liftVarToIO a = return $ a

eval :: LispEnvironment -> LispExpression -> IO LispExpression
eval _ val@(LispString _) = return val
eval env val@(LispSymbol sym) = do
  a <- findVar env (toSexp sym)
  let b = fromJust a
  return b

eval envRef (LispList[(ReservedKeyword DefKeyword), LispSymbol var, form]) =
  eval envRef form >>= defineVar envRef (toSexp var)

eval envRef (LispList (LispSymbol func: args)) =
  mapM (eval envRef) args >>= builtInOp func

eval _ val@(LispNumber _) = return val
eval _ val@(LispBool _) = return val

evalAndPrint :: LispEnvironment -> String -> IO ()
evalAndPrint envRef expr = do
  let read = readExpression expr
      evaled = eval envRef read
  s <- liftM show evaled
  print s

repl :: IO ()
repl = freshEnv >>= untilM (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

untilM :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> untilM pred prompt action
