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
    return list

item :: Parsec String () Item
item = itemSimpleProcess
    <|> itemProcess
    <|> itemSimpleData
    <|> itemData
    <|> itemLink

itemSimpleProcess :: Parsec String () Item
itemSimpleProcess = do
    try $
        reserved "process"
    id <- identifier
    try $
        reserved ";"
    return $ Process id id id

itemProcess :: Parsec String () Item
itemProcess = do
    try $
        reserved "process"
    id <- identifier
    label <- stringL
    name <- stringL
    try $
        reserved ";"
    return $ Process id label name

itemSimpleData :: Parsec String () Item
itemSimpleData = do
    try $
        reserved "data"
    id <- identifier
    try $
        reserved ";"
    return $ Data id id id

itemData :: Parsec String () Item
itemData = do
    try $
        reserved "data"
    id <- identifier
    label <- stringL
    name <- stringL
    try $
        reserved ";"
    return $ Data id label name

itemLink :: Parsec String () Item
itemLink = do
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
