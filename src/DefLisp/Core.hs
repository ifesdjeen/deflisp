{-# LANGUAGE ViewPatterns #-}

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
fromJust Nothing = error "Maybe.fromJust: got Nothing"
fromJust (Just x) = x

fnFromString :: String -> LispExpression
fnFromString expression =
  case (readExpression expression) of
    (LispList[(ReservedKeyword FnKeyword), LispVector bindings, form]) ->
      LispFunction $ UserFunction [] bindings form
    _ -> error ""

freshEnv :: IO LispEnvironment
freshEnv = do
  env <- H.new
  void $ defineVar env (LispSymbol "+") (LispFunction $ LibraryFunction "+" (builtInOp "+"))
  void $ defineVar env (LispSymbol "-") (LispFunction $ LibraryFunction "-" (builtInOp "-"))
  void $ defineVar env (LispSymbol "*") (LispFunction $ LibraryFunction "*" (builtInOp "*"))
  void $ defineVar env (LispSymbol "/") (LispFunction $ LibraryFunction "/" (builtInOp "/"))
  void $ defineVar env (LispSymbol "first") (LispFunction $ LibraryFunction "first" (builtInOp "first"))
  void $ defineVar env (LispSymbol "next") (LispFunction $ LibraryFunction "next" (builtInOp "next"))
  void $ defineVar env (LispSymbol "last") (LispFunction $ LibraryFunction "last" (builtInOp "last"))
  void $ defineVar env (LispSymbol "conj") (LispFunction $ LibraryFunction "conj" (builtInOp "conj"))
  void $ defineVar env (LispSymbol "cons") (LispFunction $ LibraryFunction "cons" (builtInOp "cons"))
  void $ defineVar env (LispSymbol "=") (LispFunction $ LibraryFunction "+" (builtInOp "="))
  void $ defineVar env (LispSymbol "list") (LispFunction $ LibraryFunction "list" (builtInOp "list"))
  void $ defineVar env (LispSymbol "empty?") (fnFromString "(fn [a] (= a ()))")
  void $ defineVar env (LispSymbol "inc") (fnFromString "(fn [b] (+ b 1))")
  void $ defineVar env (LispSymbol "map") (fnFromString "(fn [f coll] (if (empty? coll) (quote ()) (cons (f (first coll)) (map f (next coll)))))")
  void $ defineVar env (LispSymbol "reduce") (fnFromString "(fn [f coll acc] (if (empty? coll) acc (reduce f (next coll) (f acc (first coll)))))")
  return env

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


isJust         :: Maybe a -> IO Bool
isJust Nothing = return False
isJust _       = return True

findOne :: LispExpression -> LispEnvironment -> IO (Maybe LispExpression)
findOne symbol env = H.lookup env symbol

findVarInClosure :: [LispEnvironment] -> LispExpression -> IO (Maybe LispExpression)

findVarInClosure envs symbol | trace ("findvarinclosure envsCount:" ++ show (length envs) ++ " sym:" ++ show symbol) False = undefined

findVarInClosure envs symbol = do
  mapped <- mapM (findOne symbol) envs
  filtered <- filterM isJust mapped
  return $ head filtered
  -- return $ head $ filter isJust (map (findOne symbol) envs)

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

drop_ :: Int -> [LispExpression] -> [LispExpression]
drop_ n x = drop n x

-- vals :: LispExpression a => [a] -> [a]
-- vals (LispList x) = x

--
-- Numerical Operations

--

numericOp :: (Integer -> Integer -> Integer) -> [LispExpression] -> LispExpression
numericOp op args = LispNumber $ foldl1 op $ map unpackNum args

builtInOp :: String -> [LispExpression] -> LispExpression
-- builtInOp "+" args | trace(show $ foldl1 (+) $ map unpackNum args) False = undefined
builtInOp "list" args = LispList args
builtInOp op args | trace ("builtinop " ++ show op ++ show args) False = undefined

builtInOp "+" args = numericOp (+) args
builtInOp "-" args = numericOp (-) args
builtInOp "*" args = numericOp (*) args
builtInOp "/" args = numericOp (div) args
builtInOp "=" args = LispBool $ and $ map (== head args) (tail args)

builtInOp "first" [list] = lfirst list
builtInOp "next" [list] = next list
builtInOp "last" [list] = llast list
builtInOp "conj" [list, el] = conj list el
builtInOp "cons" [el, list] = cons el list

builtInOp _ _ = error "Builtin operation is not known"

liftVarToIO :: LispExpression -> IO LispExpression
liftVarToIO a = return $ a

getClosure :: Maybe LispEnvironment -> IO LispEnvironment
getClosure (Just x) = return x
getClosure Nothing = freshEnv

eval :: LispEnvironment -> [LispEnvironment] -> LispExpression -> IO LispExpression

eval _ _ c | trace ("eval " ++ show c) False = undefined

eval _ _ val@(LispString _) = return val

eval _ _ val@(LispNumber _) = return val

eval _ _ val@(LispBool _) = return val

eval _ _ LispNil = return LispNil

-- TODO do replacing of vars
eval env closure (LispSymbol sym) = do
  let s = (toSexp sym)
  a <- (liftM2 mplus) (findVarInClosure closure s) (findVar env s)
  return $ fromJust a

-- Defines a variable
eval env closure (LispList[(ReservedKeyword DefKeyword), LispSymbol var, form]) = do
  res <- eval env closure form
  _ <- defineVar env (toSexp var) res
  return res

-- Creates a function
--eval _ _ (LispList[(ReservedKeyword FnKeyword), LispVector(bindings:(LispSymbol "&"):varargs  ), form]) = do
eval _ closure (LispList[(ReservedKeyword FnKeyword),
                         LispVector((break (== (LispSymbol "&")) ->
                                     (bindings, _:(vararg: _)))),
                         form]) = do
  return $ LispFunction $ VarArgFunction closure bindings vararg form

eval envRef closure (LispList[(ReservedKeyword DefMacroKeyword),
                              (LispSymbol name),
                              LispVector((break (== (LispSymbol "&")) ->
                                          (bindings, _:(vararg: _)))),
                              form]) = do
  let n = (LispSymbol name)
      macro = LispFunction $ VariadicMacros bindings vararg form
  void $ defineVar envRef n macro
  return n

eval envRef closure (LispList
                     [(ReservedKeyword DefMacroKeyword),
                      (LispSymbol name),
                      (LispVector bindings),
                      form]) = do
  let n = (LispSymbol name)
      macro = LispFunction $ Macros bindings form
  void $ defineVar envRef n macro
  return n

-- Create a new function from enclosed arguments
eval _ closure (LispList[(ReservedKeyword FnKeyword), LispVector bindings, form]) = do
  return $ LispFunction $ UserFunction closure bindings form

eval env closure (LispList[(ReservedKeyword IfKeyword), testExpression,
                           truthyExpression, falsyExpression]) = do
  test <- (eval env closure testExpression)
  res <- if (isTrue test)
         then (eval env closure truthyExpression)
         else (eval env closure falsyExpression)
  return res

-- Evaluates a raw function, without binding
eval envRef closure (LispList
                     (val@(LispFunction (UserFunction fnClosure bindings form)) :
                      args)) =
  evalFn envRef closure val args

eval envRef closure (LispList
                     (val@(LispFunction
                           (VarArgFunction fnClosure bindings vararg form)) :
                      args)) =
  evalFn envRef closure val args

eval envRef closure (LispList
                     ((LispFunction (Macros bindings form)) : args)) = do
  let zipped = zip bindings args
  -- TODO Extract that to the separate function
  e <- freshEnv
  void $ defineVars e zipped
  res <- eval envRef ([e] ++ closure) form
  return $ res

eval _ _ (LispList [(LispSymbol "quote"), val]) = return val

eval envRef closure (LispList (LispSymbol func: args)) = do
  let func_ = (toSexp func)
  lookup_ <- (liftM2 mplus) (findVarInClosure closure func_) (findVar envRef func_)
  res <- case lookup_ of
    (Just x) -> evalFn envRef closure x args
    Nothing  -> error "No such function"
  return res

eval _ _ (LispList []) = return $ LispList []

eval env closure (LispList x) = do
  -- let enclosed = eval env closure
  y <- mapM (eval env closure) x
  z <- eval env closure (LispList y)
  return $ z

eval _ _ val@(LispNumber _) = return val
eval _ _ val@(LispBool _) = return val

eval _ _ _ = error "Can't eval"

evalFn :: LispEnvironment -> [LispEnvironment] -> LispExpression -> [LispExpression] -> IO LispExpression

evalFn envRef closure (LispFunction (VariadicMacros bindings vararg form)) args = do
  let zipped = zip bindings args
  e <- freshEnv
  void $ defineVars e zipped
  void $ defineVar e vararg (LispList (drop (length bindings) args))
  expansion <- eval envRef ([e] ++ closure) form
  res2 <- eval envRef ([e] ++ closure) expansion
  return $ res2

evalFn envRef closure (LispFunction (UserFunction fnClosure bindings form)) args
  | (length bindings) /= (length args) = error "Incorrect number of arguments"
  | otherwise = do let zipped = zip bindings args
                   e <- freshEnv
                   void $ defineVars e zipped
                   res <- eval envRef ([e] ++ fnClosure ++ closure) form
                   return $ res

evalFn envRef closure (LispFunction (VarArgFunction fnClosure bindings vararg form)) args
  | (length bindings) < (length args) = error "Incorrect number of arguments"
  | otherwise =  do let zipped = zip bindings args
                    e <- freshEnv
                    void $ defineVars e zipped
                    void $ defineVar e vararg (LispList (drop (length bindings) args))
                    res <- eval envRef ([e] ++ closure) form
                    return $ res

evalFn envRef closure (LispFunction (LibraryFunction _ native)) args = do
  evaledArgs <- mapM (eval envRef closure) args
  return $ native evaledArgs

evalFn _ _ _ _ = error "Function can be either native, user-defined or built-in"

evalAndPrint :: LispEnvironment -> String -> IO ()
evalAndPrint envRef expr = do
  let read_ = readExpression expr
      evaled = eval envRef [] read_
  s <- liftM show evaled
  print s

evalString :: LispEnvironment -> String -> IO LispExpression
evalString env expr = eval env [] (readExpression expr)

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
