module Main where

import           Caronch.Lang.Data
import           Caronch.Lang.Parser
import           Caronch.Writer.Graphviz

import           Data.Either
import           Data.List               hiding (find)
import           Data.List.Split
import           Data.Ord
import           System.Directory
import           System.Environment
import           System.FilePath.Find
import           System.IO
import           Text.Parsec

pattern :: String
pattern = "*.caronch"

main :: IO ()
main = do
    dir   <- getCurrentDirectory
    files <- search dir
    content <- mapM readFile files
    let objects = zipParse files content
    let all = assocSourceResult files objects
        errors = filter (\ (_,x) -> isLeft x) all
        goods = filter (\ (_,x) -> isRight x) all
    mapM_ printBoth errors
    putStrLn "* Stats"
    putStr "    * Files found: "
    print $ length files
    putStr "    * Objects found: "
    print . length . concat . rights $ objects
    putStr "    * Files with errors: "
    print $ length errors
    putStrLn "* Raw data"
    mapM_ printBoth goods
    putStrLn "* Graphviz'like"
    mapM_ printGraphviz goods

search :: FilePath -> IO [FilePath]
search = find always ( fileName ~~? pattern )

normaliseComments :: String -> String
normaliseComments = map (\ x -> if x == ';' then ';' else x)

zipParse :: [FilePath] -> [String] -> [Either ParseError [Item]]
zipParse = zipWith parseConfigFile

assocSourceResult :: [FilePath] -> [Either ParseError [Item]] -> [(FilePath, Either ParseError [Item])]
assocSourceResult = zip

printBoth :: (FilePath, Either ParseError [Item]) -> IO ()
printBoth (filepath, item) = do
    putStr "***** File: "
    putStrLn filepath
    print item
    putStrLn ""

printGraphviz :: (FilePath, Either ParseError [Item]) -> IO ()
printGraphviz (filepath, items) = do
    case items of
        Left  items -> putStrLn $ filepath ++ " ERROR"
        Right items -> putStrLn $ unlines
                [ "***** File: "
                , filepath
                , "***** Graphviz: "
                , writeGraphviz items
                , ""
                ]

printArray n arr = mapM_ ((putStrLn . unwords) . map show) $ chunksOf n arr
