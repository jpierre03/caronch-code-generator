module Main where

import          Caronch.Lang.Data
import          Caronch.Lang.Parser

import           Data.Either
import           Data.List                        hiding (find)
import           Data.List.Split
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
    let normalisedContent = map normaliseComments content
        objects = zipWith parseConfigFile files normalisedContent
    let all = zip files objects
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
    --printArray 1 content
    putStrLn "Hello !"

search :: FilePath -> IO [FilePath]
search = find always ( fileName ~~? pattern )

normaliseComments :: String -> String
normaliseComments = map (\ x -> if x == ';' then '#' else x)

printBoth :: FilePath -> Either ParseError [Item] -> IO ()
printBoth filepath item = do
    putStr "***** File: "
    putStrLn filepath
    print item
    putStrLn ""

printArray n arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf n arr
