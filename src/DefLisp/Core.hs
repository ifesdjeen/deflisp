module Deflisp.Core where

import Data.List
import Deflisp.Core.Types
import Deflisp.Core.Show
import Deflisp.Core.Parser

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

defineVars :: LispEnvironment -> [(LispExpression, LispExpression)] -> IO (LispExpression)
defineVars env bindings = do
  mapM (addBinding env) bindings
  return $ LispNumber 1
  where addBinding e (var, value) = do
          H.insert e var value
          return var


findVar :: LispEnvironment -> LispExpression -> IO (Maybe LispExpression)
findVar env symbol = H.lookup env symbol

-- bar :: IO (HashTable Int Int) ->

unpackNum :: LispExpression -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispList [n]) = unpackNum n
unpackNum (LispString s) = read s :: Integer

length_ :: [LispExpression] -> Int
length_ x = length x

-- vals :: LispExpression a => [a] -> [a]
-- vals (LispList x) = x

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

eval :: LispEnvironment -> Maybe LispEnvironment -> LispExpression -> IO LispExpression
eval _ _ val@(LispString _) = return val

-- TODO do replacing of vars
eval env closure val@(LispSymbol sym) = do
  a <- findVar env (toSexp sym)
  let b = fromJust a
  return b

eval env closure (LispList[(ReservedKeyword DefKeyword), LispSymbol var, form]) =
  eval env closure form >>= defineVar env (toSexp var)

eval envRef closure (LispList[(ReservedKeyword FnKeyword), LispVector bindings, form]) = do
  hFlush stdout
  return $ LispFunction bindings form

eval envRef closure (LispList ((LispFunction bindings form) : args)) = do
  let zipped = zip bindings args
  _ <- defineVars envRef zipped
  res <- eval envRef closure form
  return $ res

eval envRef _ (LispList [(LispSymbol "quote"), val]) = return val

eval envRef _ (LispList (LispSymbol func: args)) = do
  lookup <- findVar envRef (toSexp func)
  res <- case lookup of
    (Just x) -> evalFn envRef x args
    Nothing  -> evalBuiltin envRef (toSexp func) args
  return res


eval env closure (LispList x) = do
  let enclosed = eval env closure
  y <- mapM enclosed x
  z <- enclosed (LispList y)
  return $ z

eval _ _ val@(LispNumber _) = return val
eval _ _ val@(LispBool _) = return val

evalBuiltin :: LispEnvironment -> LispExpression -> [LispExpression] -> IO LispExpression
evalBuiltin envRef (LispSymbol func) args =
  mapM (eval envRef Nothing) args >>= builtInOp func

evalFn :: LispEnvironment -> LispExpression -> [LispExpression] -> IO LispExpression
evalFn envRef (LispFunction bindings form) args = do
  let zipped = zip bindings args
  _ <- defineVars envRef zipped
  res <- eval envRef Nothing form
  return $ res

evalAndPrint :: LispEnvironment -> String -> IO ()
evalAndPrint envRef expr = do
  let read = readExpression expr
      evaled = eval envRef Nothing read
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

-- readExpression "(fn [a b] (+ a b))"
