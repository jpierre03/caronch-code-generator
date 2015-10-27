module Caronch.Lang.Parser where

import           Caronch.Lang.Data

import           Control.Monad.Identity           (Identity)
import           Data.Char
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token                as Token

--parseConfigFile :: (FilePath, String) -> Either ParseError [Item]
parseConfigFile :: FilePath -> String -> Either ParseError [Item]
parseConfigFile filepath string = parse configFile filepath string

-- Lexer
languageDef = emptyDef { Token.commentLine = "//"
                       , Token.commentStart = "/*"
                       , Token.commentEnd = "*/"
                       , Token.identStart  = letter <|> oneOf "+_"
                       , Token.identLetter = alphaNum <|> oneOf ":!$%&*+.,/<=>?@\\^|-~_"
                       , Token.reservedNames     = ["define"]
                       }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
whiteSpace = Token.whiteSpace lexer -- parses whitespace
braces     = Token.braces     lexer

-- Parser
configFile :: Parser [Item]
configFile = do
    whiteSpace
    list <- many item
    --return $ catMaybes list
    return $ list

item :: Parsec String () Item
item = item_simple_process
    <|> item_simple_data
    <|> item_link

item_simple_process :: Parsec String () Item
item_simple_process = do
    try $ do
        reserved "process"
    id <- identifier
    return $ (SimpleProcess id)

item_simple_data :: Parsec String () Item
item_simple_data = do
    try $ do
        reserved "data"
    id <- identifier
    return $ (SimpleData id)

item_link :: Parsec String () Item
item_link = do
    try $ do
        reserved "process"
    id <- identifier
    try $ do
        reserved "->"
    id' <- identifier
    return $ (Link id id')

