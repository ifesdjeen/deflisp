{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Deflisp.CoreReader where

import Data.Generics
-- import Data.List
import Deflisp.Core.Types
import Deflisp.Core.Show
import Deflisp.Core.Parser

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq
import Control.Monad (forM_)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity
-- import Control.Monad.Error
import Debug.Trace


import qualified Data.Map as Map

fromJust :: Maybe LispExpression -> LispExpression
fromJust Nothing = error "Maybe.fromJust: got Nothing"
fromJust (Just x) = x

freshEnv :: LispEnvironment

freshEnv =
 defineVars Map.empty
  [mkFunction "+",
   mkFunction "-",
   mkFunction "*",
   mkFunction "/",
   mkFunction "=",
   mkFunction "print",
   mkFunction "first",
   mkFunction "count",
   mkFunction "next",
   mkFunction "last",
   mkFunction "conj",
   mkFunction "cons",
   -- mkFunction "list",
   mkFunction "vector"]
  where mkFunction name = ( (LispSymbol name),
                            (LispFunction $ LibraryFunction name (builtInOp name)) );

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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

extractSymbols :: LispExpression -> [LispExpression]
extractSymbols expr =
  everything (++)  ([] `mkQ` extract) expr
  where extract val@(LispSymbol symbol) = [val]
        extract _ = []


-- eval closure (LispList[(ReservedKeyword DefKeyword), var@(LispSymbol _), form]) =-
extractUnquote :: LispExpression -> [LispExpression]
extractUnquote expr =
  everything (++)  ([] `mkQ` extract) expr
  where extract (LispList [(LispSymbol "unquote"), val@(LispList _)]) = [val]
        extract _ = []

tryDefineExpression :: LispExpression -> Writer [(LispExpression, LispExpression)] ()
tryDefineExpression (LispList[(ReservedKeyword DefKeyword), var@(LispSymbol _), form]) = do
  tell [(var, form)]
  return ()

definitionPass :: [LispExpression] -> Writer [(LispExpression, LispExpression)] ()
definitionPass expressions = do
  forM_ expressions $ \expr -> do
     tryDefineExpression expr

buildEnvironment :: [(LispExpression, LispExpression)] -> LispEnvironment
buildEnvironment expressions = defineVars freshEnv expressions

eval :: MonadReader LispEnvironment m => LispExpression -> m LispExpression

eval val@(ReservedKeyword _) = return val
eval val@(LispString _) = return val
eval val@(LispNumber _) = return val
eval val@(LispBool _) = return val
eval LispNil = return LispNil

eval sym@(LispSymbol _) = do
  env <- ask
  return $ fromJust (findVar env sym)

eval (LispList
      ((LispFunction (LibraryFunction _ native)) :
       args)) =
  do
    env <- ask
    evaled <- mapM eval args
    return $! native evaled

eval (LispList (func@(LispSymbol _): args)) = do
  env <- ask
  funk <- eval func
  --- let funk = runIdentity $ (runReaderT $ eval func) env
  res <- eval (LispList ([funk] ++ args))
  --- Why can't i just write return eval (LispList ([funk] ++ args)) and have to write that intermediate thing?
  return res

eval (LispList ((LispFunction (UserFunction fnClosure bindings form)) : args))
  | (length bindings) /= (length args) = error "Incorrect number of arguments"
  | otherwise = do
    env <- ask
    evaled <- mapM eval args
    res <- local (\x -> defineVars x (zip bindings evaled)) (eval form)
    return res

-- definitionPass [LispList [(ReservedKeyword DefKeyword), (LispSymbol "asd"), (LispNumber 1)], LispList [(ReservedKeyword DefKeyword), (LispSymbol "bsd"), (LispNumber 2)]]
--- execution only through main function, no top-level expressions

-- eval freshEnv [LispList [(LispSymbol "+"), (LispNumber 1), (LispNumber 1)]]

-- [LispList [(ReservedKeyword DefKeyword), (LispSymbol "asd"), (LispNumber 1)]]



--- (runReaderT $ eval (LispNumber 1) ) freshEnv

--- (runReaderT $ eval (LispSymbol "+") ) freshEnv
--- (runReaderT $ eval (LispList [(LispSymbol "+"), (LispNumber 1), (LispNumber 1)]) ) freshEnv
