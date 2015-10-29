module Caronch.Lang.Parser where

import           Caronch.Lang.Data

import           Control.Monad.Identity (Identity)
import           Data.Char
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token      as Token

parseConfigFile :: FilePath -> String -> Either ParseError [Item]
parseConfigFile filepath string = parse configFile filepath string

-- * Lexer

languageDef = emptyDef { Token.commentLine = "//"
                       , Token.commentStart = "/*"
                       , Token.commentEnd = "*/"
                       , Token.identStart  = letter <|> oneOf "+_"
                       , Token.identLetter = alphaNum <|> oneOf ":!$%&*+.,/<=>?@\\^|-~_"
                       , Token.reservedNames     = ["data", "process", "link", ";"]
                       }

lexer = Token.makeTokenParser languageDef

identifier      = Token.identifier      lexer -- parses an identifier
reserved        = Token.reserved        lexer -- parses a reserved name
whiteSpace      = Token.whiteSpace      lexer -- parses whitespace
braces          = Token.braces          lexer
symbol          = Token.symbol          lexer

-- * Parser

configFile :: Parser [Item]
configFile = do
    whiteSpace
    list <- many item
    return $ list

item :: Parsec String () Item
item = item_simple_process
    <|> item_process
    <|> item_simple_data
    <|> item_data
    <|> item_link

item_simple_process :: Parsec String () Item
item_simple_process = do
    try $
        reserved "process"
    id <- identifier
    try $
        reserved ";"
    return $ SimpleProcess id

item_process :: Parsec String () Item
item_process = do
    try $
        reserved "process"
    id <- identifier
    label <- stringL
    name <- stringL
    try $
        reserved ";"
    return $ Process id label name

item_simple_data :: Parsec String () Item
item_simple_data = do
    try $
        reserved "data"
    id <- identifier
    try $
        reserved ";"
    return $ SimpleData id

item_data :: Parsec String () Item
item_data = do
    try $
        reserved "data"
    id <- identifier
    label <- stringL
    name <- stringL
    try $
        reserved ";"
    return $ Data id label name

item_link :: Parsec String () Item
item_link = do
    try $
        reserved "link"
    id <- identifier
    try $
        reserved "->"
    id' <- identifier
    try $
        reserved ";"
    return $ Link id id'

stringL = do
    char '\"' <|> char '"'
    x <- many (noneOf "\"")
    char '\"' <|> char '"'
    --char '\"'
    return x
