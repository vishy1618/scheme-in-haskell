module Lib
    ( readExpression
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)

parseSymbol :: Parser Char
parseSymbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- unused
spacesParser :: Parser ()
spacesParser = skipMany1 space

spacesPrecedingSymbolParser :: Parser Char
spacesPrecedingSymbolParser = spacesParser >> parseSymbol
-- unused

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
    everythingElse <- many (letter <|> parseSymbol <|> digit)
    let atom = firstCharacter:everythingElse
    return $ case atom of
        "#t"    -> Bool True
        "#f"    -> Bool False
        _       -> Atom atom

numberParser :: Parser LispVal
numberParser = do
   numberAsString <- many1 digit
   return $ Number $ read numberAsString

lispExpressionParser :: Parser LispVal
lispExpressionParser = atomParser <|> stringParser <|> numberParser

readExpression :: String -> String
readExpression input = case (parse lispExpressionParser "lisp" input) of
    Left err -> "No match: " ++ show err
    Right val -> case val of
        String str  -> "Found string: " ++ str
        Atom str    -> "Found atom: " ++ str
        Bool bool   -> "Found bool: " ++ (show bool)
        Number num  -> "Found number: " ++ (show num)
        _           -> "Found value"
