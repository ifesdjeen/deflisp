module IKnow.Core where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State

import IKnow.Core.Types
import Debug.Trace

import Text.ParserCombinators.Parsec

symbols :: Parser Char
symbols = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces_ :: Parser ()
spaces_ = skipMany1 space

parseString :: Parser LispExpression
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <-char '"'
                 return $ LispString x

parseNumber :: Parser LispExpression
parseNumber = fmap (LispNumber . read) $ many digit

parseSymbol :: Parser LispExpression
parseSymbol = do
  first <- letter <|> symbols
  rest <- many (letter <|> digit <|> symbols)
  let symbol = first:rest
  return $ LispSymbol symbol

parseList :: Parser LispExpression
parseList = do
  _ <- char '('
  x <- parseLispExpression `sepEndBy` (many1 space)
  _ <- char ')'
  return $ LispList x

parseLispExpression :: Parser LispExpression
parseLispExpression = parseSymbol
                  <|> parseString
                  <|> parseList
                  <|> parseNumber

unwordsList :: [LispExpression] -> String
unwordsList = unwords . map showVal -- ()

showVal :: LispExpression -> String
showVal (LispNumber x) = show x
showVal (LispSymbol x) = "\'" ++ x
showVal (LispList x) = "(" ++ unwordsList x ++ ")"
showVal (LispString x) = "\"" ++ x ++ "\""
showVal (LispBool _) = ""

readExpression :: String -> LispExpression
readExpression input = case (parse parseLispExpression "lisp" input) of
  Left err -> LispString $ "No match: " ++ show err
  Right x -> x

unpackNum :: LispExpression -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispString s) = read s :: Integer

--
-- Numerical Operations
--

numericOp :: (Integer -> Integer -> Integer) -> [LispExpression] -> LispExpression
numericOp op args = LispNumber $ foldl1 op $ map unpackNum args

builtInOp :: String -> [LispExpression] -> LispExpression
-- builtInOp "+" args | trace(show $ foldl1 (+) $ map unpackNum args) False = undefined
builtInOp "+" args = numericOp (+) args
builtInOp "-" args = numericOp (-) args
builtInOp "*" args = numericOp (*) args
builtInOp "/" args = numericOp (div) args

eval :: LispExpression -> LispExpression
eval val@(LispString _) = val
eval val@(LispSymbol _) = val
-- eval val@(LispList _) = val
-- eval (LispList (LispSymbol _: args)) = LispNumber $ foldl1 (+) $ map unpackNum args
eval (LispList (LispSymbol func: args)) = builtInOp func args
eval val@(LispNumber _) = val
eval val@(LispBool _) = val

-- evalExpression :: String -> String
