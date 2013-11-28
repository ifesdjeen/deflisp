module Deflisp.Core where

-- import Data.List
import Deflisp.Core.Types
import Deflisp.Core.Show
import Deflisp.Core.Parser

import System.IO
import Control.Monad.Error
import Debug.Trace

import qualified Data.HashTable.IO as H

type IOThrowsError = ErrorT LispError IO

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing YOYO"
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
  void $ mapM (addBinding env) bindings
  return $ LispNumber 1
  where addBinding e (var, value) = do
          H.insert e var value
          return var



findVarMaybe :: Maybe LispEnvironment -> LispExpression -> IO (Maybe LispExpression)
findVarMaybe env symbol =
  case env of
    (Just x) -> H.lookup x symbol
    Nothing -> return Nothing

findVar :: LispEnvironment -> LispExpression -> IO (Maybe LispExpression)
findVar env symbol = H.lookup env symbol

-- bar :: IO (HashTable Int Int) ->

unpackNum :: LispExpression -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispList [n]) = unpackNum n
unpackNum (LispString s) = read s :: Integer
unpackNum nan = error $ "Can't unpack number: " ++ (show nan)

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
builtInOp "=" args = return $ LispBool $ and $ map (== head args) (tail args)

builtInOp "first" [list] = return $ lfirst list
builtInOp "next" [list] = return $ next list
builtInOp "last" [list] = return $ llast list
builtInOp "conj" [list, el] = return $ conj list el
builtInOp "cons" [el, list] = return $ cons el list

liftVarToIO :: LispExpression -> IO LispExpression
liftVarToIO a = return $ a

eval :: LispEnvironment -> Maybe LispEnvironment -> LispExpression -> IO LispExpression

eval _ _ c | trace ("eval " ++ show c) False = undefined

eval _ _ val@(LispString _) = return val

eval _ _ val@(LispNumber _) = return val

eval _ _ val@(LispBool _) = return val

-- TODO do replacing of vars
eval env maybeClosure val@(LispSymbol sym) = do
  let s = (toSexp sym)
  --- WHY?
  --a <- (findVarMaybe maybeClosure s) `mplus` (findVar env s)
  a <- (liftM2 mplus) (findVarMaybe maybeClosure s) (findVar env s)
  return $ fromJust a

-- Defines a variable
eval env closure (LispList[(ReservedKeyword DefKeyword), LispSymbol var, form]) = do
  res <- eval env closure form
  _ <- defineVar env (toSexp var) res
  return res

-- Creates a function
eval _ _ (LispList[(ReservedKeyword FnKeyword), LispVector bindings, form]) = do
  --- TODO: add closure enclosing to function creation!!! Currently state is lost.
  return $ LispFunction bindings form

-- eval env closure (LispList[(ReservedKeyword IfKeyword), testExpression,
--                            truthyExpression, falsyExpression]) | trace ("here" ) False = undefined

eval env closure (LispList[(ReservedKeyword IfKeyword), testExpression,
                           truthyExpression, falsyExpression]) = do
  test <- (eval env closure testExpression)
  res <- if (isTrue test)
         then (eval env closure truthyExpression)
         else (eval env closure falsyExpression)
  return res




-- Evaluates a raw function, without binding
eval envRef closure (LispList ((LispFunction bindings form) : args)) = do
  let zipped = zip bindings args
  e <- case closure of
        (Just x) -> return x
        Nothing -> freshEnv
  _ <- defineVars e zipped
  res <- eval envRef (Just e) form
  return $ res
  -- Why on earth wouldn't that work?.... So that one should work, te
  -- where enclosedEnv = case closure of
  --         (Just x) -> closure
  --          Nothing -> maybe $ join freshEnv


eval _ _ (LispList [(LispSymbol "quote"), val]) = return val

eval envRef closure (LispList (LispSymbol func: args)) = do
  -- lookup_ <- findVar envRef (toSexp func)
  let func_ = (toSexp func)
  lookup_ <- (liftM2 mplus) (findVarMaybe closure func_) (findVar envRef func_)
  evaledArgs <- mapM (eval envRef closure) args
  res <- case lookup_ of
    (Just x) -> evalFn envRef closure x evaledArgs
    Nothing  -> builtInOp func evaledArgs
  return res


eval env closure (LispList x) = do
  -- let enclosed = eval env closure
  y <- mapM (eval env closure) x
  z <- eval env closure (LispList y)
  return $ z

eval _ _ val@(LispNumber _) = return val
eval _ _ val@(LispBool _) = return val

evalFn :: LispEnvironment -> Maybe LispEnvironment -> LispExpression -> [LispExpression] -> IO LispExpression
evalFn envRef closure (LispFunction bindings form) args = do
  let zipped = zip bindings args
  e <- case closure of
    (Just x) -> return x
    Nothing -> freshEnv
  _ <- defineVars e zipped
  res <- eval envRef (Just e) form
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
untilM pred_ prompt action = do
  result <- prompt
  if pred_ result
     then return ()
     else action result >> untilM pred_ prompt action

-- readExpression "(fn [a b] (+ a b))"


-- (def b 100)
-- (def a (fn [b c] (+ b c)))
-- (a 5 7)

--  Nothing `mplus` Nothing `mplus` Just 1 `mplus` Just 2
-- First foldMap [Nothing, Nothing, Just 1, Just 2]

(def empty? (fn [a] (= a (quote ()))))
(def inc (fn [b] (+ b 1)))
(def map (fn [f coll] (if (empty? coll) (quote ()) (cons (f (first coll)) (map f (next coll))))))

-- ((fn [f sm] (f sm)) (fn [b] (+ b 1)))
