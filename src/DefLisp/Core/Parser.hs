module DefLisp.Core.Parser (readExpression) where

import DefLisp.Core.Types

import Text.ParserCombinators.Parsec

symbols :: Parser Char
symbols = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces_ :: Parser ()
spaces_ = skipMany1 space

parseString :: Parser LispExpression
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ LispString x

parseNumber :: Parser LispExpression
parseNumber = fmap (LispNumber . read) $ many digit

parseReserved :: Parser LispExpression
parseReserved = do
  res <-  string "def" <|>
          string "if"
  return $ toSexp res

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
parseLispExpression = parseReserved <|>
                      parseSymbol <|>
                      parseString <|>
                      parseList <|>
                      parseNumber

readExpression :: String -> LispExpression
readExpression input = case (parse parseLispExpression "lisp" input) of
  Left err -> LispString $ "No match: " ++ show err
  Right x -> x
