{-# LANGUAGE ViewPatterns #-}

module Deflisp.Core where

-- import Data.List
import Deflisp.Core.Types
import Deflisp.Core.Show
import Deflisp.Core.Parser

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Debug.Trace

import qualified Data.HashTable.IO as H
import qualified Data.Map as Map
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

freshEnv :: LispEnvironment
--freshEnv = Map.empty
freshEnv =
  defineVars Map.empty
  [( (LispSymbol "+"), (LispFunction $ LibraryFunction "+" (builtInOp "+")) ),
   ( (LispSymbol "+"), (LispFunction $ LibraryFunction "+" (builtInOp "+")) ),
   ( (LispSymbol "-"), (LispFunction $ LibraryFunction "-" (builtInOp "-")) ),
   ( (LispSymbol "*"), (LispFunction $ LibraryFunction "*" (builtInOp "*")) ),
   ( (LispSymbol "/"), (LispFunction $ LibraryFunction "/" (builtInOp "/")) ),
   ( (LispSymbol "="), (LispFunction $ LibraryFunction "+" (builtInOp "=")) ),
   ( (LispSymbol "first"), (LispFunction $ LibraryFunction "first" (builtInOp "first")) ),
   ( (LispSymbol "next"), (LispFunction $ LibraryFunction "next" (builtInOp "next")) ),
   ( (LispSymbol "last"), (LispFunction $ LibraryFunction "last" (builtInOp "last")) ),
   ( (LispSymbol "conj"), (LispFunction $ LibraryFunction "conj" (builtInOp "conj")) ),
   ( (LispSymbol "cons"), (LispFunction $ LibraryFunction "cons" (builtInOp "cons")) ),
   ( (LispSymbol "list"), (LispFunction $ LibraryFunction "list" (builtInOp "list")) ),
   ( (LispSymbol "empty?"), (fnFromString "(fn [a] (= a ()))") ),
   ( (LispSymbol "inc"), (fnFromString "(fn [b] (+ b 1))") ),
   ( (LispSymbol "map"), (fnFromString "(fn [f coll] (if (empty? coll) (quote ()) (cons (f (first coll)) (map f (next coll)))))") ),
   ( (LispSymbol "reduce"), (fnFromString "(fn [f coll acc] (if (empty? coll) acc (reduce f (next coll) (f acc (first coll)))))") )
  ]

defineVar :: LispEnvironment -> LispExpression -> LispExpression -> LispEnvironment
defineVar env symbol expr = Map.insert symbol expr env

defineVars :: LispEnvironment -> [(LispExpression, LispExpression)] -> LispEnvironment

defineVars env ((var, value):more) =
  defineVars (Map.insert var value env) more

defineVars env [] = env

isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

findOne :: LispExpression -> LispEnvironment -> Maybe LispExpression
findOne symbol env = Map.lookup symbol env

findVarInClosure :: [LispEnvironment] -> LispExpression -> Maybe LispExpression

findVarInClosure envs symbol | trace ("findvarinclosure envsCount:" ++ show (length envs) ++ " sym:" ++ show symbol) False = undefined

findVarInClosure envs symbol =
  case (filter isJust (map (findOne symbol) envs)) of
    [] -> Nothing
    (a:_) -> a

findVar :: LispEnvironment -> LispExpression -> Maybe LispExpression

findVar _ symbol | trace ("findVar: " ++ show symbol) False = undefined

findVar env symbol = Map.lookup symbol env

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

liftEnvToIO :: LispEnvironment -> IO LispEnvironment
liftEnvToIO a = return a


getClosure :: Maybe LispEnvironment -> LispEnvironment
getClosure (Just x) = x
getClosure Nothing = freshEnv

eval :: [LispEnvironment] -> LispExpression -> State LispEnvironment LispExpression

eval _ c | trace ("eval " ++ show c) False = undefined

eval _ val@(LispString _) = return val

eval _ val@(LispNumber _) = return val

eval _ val@(LispBool _) = return val

eval _ LispNil = return LispNil

-- -- TODO do replacing of vars
eval _ (LispList [(LispSymbol "quote"), val]) = return val

eval closure sym@(LispSymbol _) = do
  env <- get
  case (findVarInClosure closure sym) of
    (Just x) -> return x
    Nothing -> return $ fromJust (findVar env sym)

eval _ (LispList []) = return $ LispList []

-- Defines a variable
eval closure (LispList[(ReservedKeyword DefKeyword), var@(LispSymbol _), form]) =
  do
    env <- get
    -- void $ defineVar env var (eval env closure form)
    evaled <- eval closure form
    put $ defineVar env var evaled
    return var

-- Creates a function
eval closure (LispList[(ReservedKeyword FnKeyword),
                       LispVector((break (== (LispSymbol "&")) ->
                                   (bindings, _:(vararg: _)))),
                       form]) =
  return $ LispFunction $ VarArgFunction closure bindings vararg form

-- Defines a variadic macros
eval closure (LispList[(ReservedKeyword DefMacroKeyword),
                       name@(LispSymbol _),
                       LispVector((break (== (LispSymbol "&")) ->
                                   (bindings, _:(vararg: _)))),
                       form]) = do
  env <- get
  put $ defineVar env name (LispFunction $ VariadicMacros bindings vararg form)
  return name

-- Creates a macros
eval closure (LispList
              [(ReservedKeyword DefMacroKeyword),
               name@(LispSymbol _),
               (LispVector bindings),
               form]) = do
  env <- get
  put $ defineVar env name (LispFunction $ Macros bindings form)
  return name

-- Create a new function from enclosed arguments
eval closure (LispList[(ReservedKeyword FnKeyword), LispVector bindings, form]) = do
  return $ LispFunction $ UserFunction closure bindings form

eval closure (LispList[(ReservedKeyword IfKeyword), testExpression,
                       truthyExpression, falsyExpression]) = do
  test <- (eval closure testExpression)
  res <- if (isTrue test)
         then (eval closure truthyExpression)
         else (eval closure falsyExpression)
  return res

-- Evaluates a raw function, without binding
eval closure (LispList
              ((LispFunction (UserFunction fnClosure bindings form)) :
               args))
  | (length bindings) /= (length args) = error "Incorrect number of arguments"
  | otherwise =
    do
      env <- get
      let evaled = map (\arg -> evalState (eval closure arg) env) args
          fnArgs = zip bindings evaled
          fnEnv = defineVars freshEnv fnArgs
      eval ([fnEnv] ++ fnClosure ++ closure) form

-- Evaluates a vararg function
eval closure (LispList
              ((LispFunction (VarArgFunction fnClosure bindings vararg form)) :
               args))
  | (length bindings) < (length args) = error "Incorrect number of arguments"
  | otherwise =
    do
      env <- get
      let evaled = map (\arg -> evalState (eval closure arg) env) args
          fnArgs = zip bindings evaled
          fnEnv = defineVars freshEnv fnArgs
          withVariadicBinding = defineVar fnEnv vararg (LispList (drop (length bindings) args))
      eval ([withVariadicBinding] ++ fnClosure ++ closure) form

-- Evaluates a library/native function
eval closure (LispList
              ((LispFunction (LibraryFunction _ native)) :
               args)) =
  -- TODO: conut arguments of a library function
  do
    env <- get
    let evaled = map (\arg -> evalState (eval closure arg) env) args
    return $ native evaled

eval closure (LispList (func@(LispSymbol _): args)) = do
  env <- get
  let funk = evalState (eval closure func) env
  eval closure (LispList ([funk] ++ args))

-- Evaluates a raw macros, without binding
eval closure (LispList
              ((LispFunction (Macros bindings form)) :
               args))
  | (length bindings) /= (length args) = error "Incorrect number of arguments"
  | otherwise =
    do
      env <- get
      let macroArgs = zip bindings args
          macroEnv = defineVars freshEnv macroArgs
      eval ([macroEnv] ++ closure) form

-- Evaluates a vararg function
eval closure (LispList
              ((LispFunction (VariadicMacros bindings vararg form)) :
               args))
  | (length bindings) < (length args) = error "Incorrect number of arguments"
  | otherwise =
    do
      env <- get
      let fnArgs = zip bindings args
          fnEnv = defineVars freshEnv fnArgs
          withVariadicBinding = defineVar fnEnv vararg (LispList (drop (length bindings) args))
      eval ([withVariadicBinding] ++ closure) form

-- eval env closure (LispList x) = do
--   -- let enclosed = eval env closure
--   y <- mapM (eval env closure) x
--   z <- eval env closure (LispList y)
--   return $ z

eval _ form = error $ "Don't know how to eval" ++ (show form)

-- evalFn _ _ _ _ = error "Function can be either native, user-defined or built-in"

repl :: IO ()
repl = repl2 freshEnv

repl2 :: LispEnvironment -> IO ()
repl2 env = do
  expression <- readPrompt "Lisp >>>"
  if (expression == "quit")
    then return ()
    else do let read_ = readExpression expression
                (result, newEnv) = runState (eval [] read_) env
            void $ print result
            repl2 newEnv

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
