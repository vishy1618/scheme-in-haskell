module Lib
    ( readExpression
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List (isPrefixOf, stripPrefix)
import Numeric (readOct, readHex)

parseSymbol :: Parser Char
parseSymbol = oneOf "!$%&|*+-/:<=>?@^_~"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

escapedCharacterParser :: Parser Char
escapedCharacterParser = do
    char '\\'
    escapedCharacter <- oneOf "\\\"nrt"
    return $ case escapedCharacter of
        '\\'    -> escapedCharacter
        '"'     -> escapedCharacter
        'n'     -> '\n'
        'r'     -> '\r'
        't'     -> '\t'

stringParser :: Parser LispVal
stringParser = do
    char '"'
    x <- many $ escapedCharacterParser <|> noneOf "\""
    char '"'
    return (String x)

atomParser :: Parser LispVal
atomParser = do
    firstCharacter <- letter <|> parseSymbol
    restOfString <- many (letter <|> parseSymbol <|> digit)
    let atom = firstCharacter:restOfString
    return $ Atom atom

octalParser :: Parser LispVal
octalParser = do
    try $ string "#o"
    octalAsString <- many1 octDigit
    return $ case (readOct octalAsString) of
        [(number, _)]   -> Number number
        _               -> Atom octalAsString

hexParser :: Parser LispVal
hexParser = do
    try $ string "#x"
    hexAsString <- many1 hexDigit
    return $ case (readHex hexAsString) of
        [(number, _)]   -> Number number
        _               -> Atom hexAsString

decimalParser :: Parser LispVal
decimalParser = do
    numberAsString <- many1 digit
    return $ Number $ read numberAsString

decimalWithNotationParser :: Parser LispVal
decimalWithNotationParser = do
    try $ string "#d"
    digits <- many1 digit
    return $ Number $ read digits

numberParser :: Parser LispVal
numberParser = octalParser <|> hexParser <|> decimalWithNotationParser <|> decimalParser

boolParser :: Parser LispVal
boolParser = trueParser <|> falseParser
    where
        trueParser = do
            try $ string "#t"
            return $ Bool True
        falseParser = do
            try $ string "#f"
            return $ Bool False

lispExpressionParser :: Parser LispVal
lispExpressionParser = atomParser <|> boolParser <|> stringParser <|> numberParser

readExpression :: String -> String
readExpression input = case (parse lispExpressionParser "lisp" input) of
    Left err -> "No match: " ++ show err
    Right val -> case val of
        String str  -> "Found string: " ++ str
        Atom str    -> "Found atom: " ++ str
        Bool bool   -> "Found bool: " ++ (show bool)
        Number num  -> "Found number: " ++ (show num)
        _           -> "Found value"
