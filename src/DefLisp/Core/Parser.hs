module Deflisp.Core.Parser (readExpression) where

import System.IO

import Deflisp.Core.Types
import Control.Monad.Error

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

symbols :: Parser Char
symbols = oneOf "!#$%&|*+/:<=>?@^_~"

spaces_ :: Parser ()
spaces_ = skipMany1 space

parseString :: Parser LispExpression
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ LispString x

parseNumber :: Parser LispExpression
--parseNumber = liftM (LispNumber . read) $ many digit
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
     then pzero
     else return $ (LispNumber . read) $ sign ++ num

parseReserved :: Parser LispExpression
parseReserved = do
  res <-  string "def" <|>
          string "if"  <|>
          string "fn"
  return $ toSexp res

parseSymbol :: Parser LispExpression
parseSymbol = do
  first <- letter <|> symbols
  rest <- many (letter <|> digit <|> symbols)
  let symbol = first:rest
  return $ LispSymbol symbol

parseList :: Parser LispExpression
parseList = liftM LispList $ sepBy parseLispExpression whiteSpace

parseVector :: Parser LispExpression
parseVector = liftM LispVector $ sepBy parseLispExpression whiteSpace

parseLispExpression :: Parser LispExpression
parseLispExpression = parseReserved <|>
                      parseSymbol <|>
                      lexeme parseNumber <|>
                      parseString <|>
                      parens parseList <|>
                      brackets parseVector  --  <|>
                      -- <?> "Expression"

readExpression :: String -> LispExpression
readExpression input = case (parse parseLispExpression "lisp" input) of
  Left err -> LispString $ "No match: " ++ show err
  Right x -> x


lispDef :: LanguageDef ()
lispDef
  = emptyDef
  { P.commentLine    = ";"
  , P.identStart     = letter <|> symbols
  , P.identLetter    = letter <|> digit <|> symbols
  , P.reservedNames  = []
  , P.caseSensitive  = True
  }

--lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser lispDef

parens = P.parens lexer
brackets = P.brackets lexer
whiteSpace = P.whiteSpace lexer

lexeme = P.lexeme lexer
