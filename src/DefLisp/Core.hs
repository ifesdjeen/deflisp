{-# LANGUAGE ViewPatterns #-}

module Deflisp.Core where

-- import Data.List
import Deflisp.Core.Types
import Deflisp.Core.Show
import Deflisp.Core.Parser

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Error
import Debug.Trace


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
   ( (LispSymbol "="), (LispFunction $ LibraryFunction "=" (builtInOp "=")) ),
   ( (LispSymbol "print"), (LispFunction $ LibraryFunction "print" (builtInOp "print")) ),
   ( (LispSymbol "first"), (LispFunction $ LibraryFunction "first" (builtInOp "first")) ),
   ( (LispSymbol "count"), (LispFunction $ LibraryFunction "count" (builtInOp "count")) ),
   ( (LispSymbol "next"), (LispFunction $ LibraryFunction "next" (builtInOp "next")) ),
   ( (LispSymbol "last"), (LispFunction $ LibraryFunction "last" (builtInOp "last")) ),
   ( (LispSymbol "conj"), (LispFunction $ LibraryFunction "conj" (builtInOp "conj")) ),
   ( (LispSymbol "cons"), (LispFunction $ LibraryFunction "cons" (builtInOp "cons")) ),
   ( (LispSymbol "list"), (LispFunction $ LibraryFunction "list" (builtInOp "list")) ),
   ( (LispSymbol "vector"), (LispFunction $ LibraryFunction "vector" (builtInOp "vector")) )]

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

-- findVarInClosure envs symbol | trace ("findvarinclosure envsCount:" ++ show (length envs) ++ " sym:" ++ show symbol) False = undefined

findVarInClosure envs symbol =
  case (filter isJust (map (findOne symbol) envs)) of
    [] -> Nothing
    (a:_) -> a

findVar :: LispEnvironment -> LispExpression -> Maybe LispExpression

-- findVar _ symbol | trace ("findVar: " ++ show symbol) False = undefined

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

ioToWrapped :: (IO ()) -> LispExpression
ioToWrapped a = LispIO a

numericOp :: (Integer -> Integer -> Integer) -> [LispExpression] -> LispExpression
numericOp op args = LispNumber $ foldl1 op $ map unpackNum args

builtInOp :: String -> [LispExpression] -> LispExpression
-- builtInOp "+" args | trace(show $ foldl1 (+) $ map unpackNum args) False = undefined
builtInOp "list" args = LispList args
builtInOp "vector" args = LispVector args

-- builtInOp op args | trace ("builtinop " ++ show op ++ show args) False = undefined

builtInOp "+" args = numericOp (+) args
builtInOp "-" args = numericOp (-) args
builtInOp "*" args = numericOp (*) args
builtInOp "/" args = numericOp (div) args
-- TODO: Figure out how to rewrite that to LispIO
builtInOp "print" args = unsafePerformIO $ do
  void $! flushStr $ "> " ++ (unwords (map show args)) ++ "\n"
  return $! LispNil

builtInOp "=" args = LispBool $ and $ map (== head args) (tail args)

builtInOp "first" [list] = lfirst list
builtInOp "count" [list] = count list

builtInOp "next" [list] = next list
builtInOp "last" [list] = llast list
builtInOp "conj" [list, el] = conj list el
builtInOp "cons" [el, list] = cons el list

builtInOp _ _ = error "Builtin operation is not known"

eval :: [LispEnvironment] -> LispExpression -> State LispEnvironment LispExpression

-- eval _ c | trace ("eval " ++ show c) False = undefined

eval _ val@(ReservedKeyword _) = return val

eval _ val@(LispString _) = return val

eval _ val@(LispNumber _) = return val

eval _ val@(LispBool _) = return val

eval _ LispNil = return LispNil

-- eval _ (LispList [(LispSymbol "quote"), val]) | trace ("eval Quoting of " ++ show val) False = undefined
eval _ (LispList [(LispSymbol "quote"), val]) = return val

-- eval closure (LispList ((LispSymbol "do"): forms)) =
  -- do
  --   env <- get
  --   let evaled = map (\arg -> evalState (eval closure arg) env) forms
  --   return $ deepseq evaled (last evaled)

-- eval closure (LispList ((LispSymbol "do"): (x:xs))) =
--   step xs (eval closure x)
--   where step (x:more) res = deepseq res (step more (eval closure x))
--         step [] res = deepseq res res

eval closure (LispList ((LispSymbol "do"): forms)) =
  do
    env <- get
    let (res, newEnv) = evalMany env closure forms
    put $ newEnv
    return $ res
  -- do
  --   env <- get
  --   let (res, newEnv) = step forms (LispNil, env)
  --   put $ newEnv
  --   return res
  -- where step [] (lastResult, env) =
  --         (lastResult, env)
  --       step (current:more) (lastResult, env) =
  --         deepseq lastResult (step more (runState (eval closure current) env))

eval closure (LispList [(LispSymbol "eval"), form]) =
  do
    env <- get
    -- void $ defineVar env var (eval env closure form)
    let evaled = evalState (eval closure form) env
    eval closure evaled


-- eval closure (LispSymbol val) | trace ("eval Lookup of " ++ show val) False = undefined

eval closure sym@(LispSymbol _) = do
  env <- get
  case (findVarInClosure closure sym) of
    (Just x) -> return x
    Nothing -> return $ fromJust (findVar env sym)

eval _ (LispList []) = return $ LispList []

eval _ val@(LispVector _) = return val


eval closure (LispList[(ReservedKeyword DefKeyword), (LispSymbol val), _]) | trace ("eval def  " ++ show val) False = undefined

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
eval _ (LispList[(ReservedKeyword DefMacroKeyword),
                 name@(LispSymbol _),
                 LispVector((break (== (LispSymbol "&")) ->
                             (bindings, _:(vararg: _)))),
                 form]) = do
  env <- get
  put $ defineVar env name (LispFunction $ VariadicMacros bindings vararg form)
  return name

-- Creates a macros
eval _ (LispList
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
      -- let (evaled, newEnv) = evalAll env closure args
      let evaled = map (\arg -> evalState (eval closure arg) env) args
          fnArgs = zip bindings evaled
          fnEnv = defineVars freshEnv fnArgs
      -- put $ newEnv
      eval ([fnEnv] ++ fnClosure ++ closure) form

-- Evaluates a vararg function
eval closure (LispList
              ((LispFunction (VarArgFunction fnClosure bindings vararg form)) :
               args))
  | (length bindings) > (length args) = error "Incorrect number of arguments"
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
    return $! native evaled

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
      let macroArgs = zip bindings args
          macroEnv = defineVars freshEnv macroArgs
      -- eval ([macroEnv] ++ closure) form
      env <- get
      let (expanded, newEnv) = runState (eval ([macroEnv] ++ closure) form) env
      put $ newEnv
      eval closure expanded

-- Evaluates a vararg function
-- TODO: add function that would expand a macro
eval closure (LispList
              ((LispFunction (VariadicMacros bindings vararg form)) :
               args))
  | (length bindings) > (length args) = error "Incorrect number of arguments"
  | otherwise =
    do
      let fnArgs = zip bindings args
          fnEnv = defineVars freshEnv fnArgs
          withVariadicBinding = defineVar fnEnv vararg (LispList (drop (length bindings) args))
      -- eval ([withVariadicBinding] ++ closure) form
      env <- get
      let (expanded, newEnv) = runState (eval ([withVariadicBinding] ++ closure) form) env
      put $ newEnv
      eval closure expanded

-- eval _ (LispList x) | trace ("eval List: " ++ show x) False = undefined
eval closure val@(LispList x) =
  if (isPrimitive val)
  then return val
  else
    do
      env <- get
      let (res, newEnv) = evalMany env closure x
      put $ newEnv
      return $ res

-- do
    --   env <- get
    --   let evaled = LispList $ map (\arg -> evalState (eval closure arg) env) x
    --   eval closure evaled

eval _ form = error $ "Don't know how to eval :`" ++ (show form)

-- evalFn _ _ _ _ = error "Function can be either native, user-defined or built-in"

repl :: IO ()
repl = repl2 freshEnv

repl2 :: LispEnvironment -> IO ()
repl2 env = do
  expression <- readPrompt "Lisp >>> "
  if (expression == "quit")
    then return ()
    else do let read_ = readExpression expression
                (result, newEnv) = runState (eval [] read_) env
            void $ print result
            repl2 newEnv

evalString :: String -> LispExpression
evalString expression =
  evalState (step expression) freshEnv
  where step expression = do let read_ = readExpression expression
                             eval [] read_

evalStrings :: [String] -> LispExpression
evalStrings expressions =
  evalExpressions $ map readExpression expressions

evalExpressions :: [LispExpression] -> LispExpression
evalExpressions expressions =
  step expressions (LispNil, freshEnv)
  where step [] (lastResult, env) =
          lastResult
        step (current:more) (_, env) =
          step more (runState (eval [] current) env)

evalExpressionsIO :: [LispExpression] -> IO LispExpression
evalExpressionsIO expressions = return $ evalExpressions expressions


evalFile :: String -> IO LispExpression
evalFile filename = do
  s <- readFile filename
  expressions <- readExpressions s
  res <- evalExpressionsIO expressions
  return res

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



-- (defmacro or [cond & conds] (list 'if  cond cond  (if (= conds ()) 'false (cons 'or conds))))

evalMany :: LispEnvironment -> [LispEnvironment] -> [LispExpression] -> (LispExpression, LispEnvironment)
evalMany e closure forms =
  step forms (LispNil, e)
  where step [] (lastResult, env) =
          (lastResult, env)
        step (current:more) (lastResult, env) =
          deepseq lastResult (step more (runState (eval closure current) env))

evalAll :: LispEnvironment -> [LispEnvironment] -> [LispExpression] -> ([LispExpression], LispEnvironment)
evalAll env closure forms =
  step forms ([], env)
  where step [] (lastResult, env)  =
          (lastResult, env)
        step (current:more) (lastResult, env) =
          let (currentResult, newEnv) = runState (eval closure current) env in
          step more ([deepseq currentResult currentResult] ++ lastResult, newEnv)
