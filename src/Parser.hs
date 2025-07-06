{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

data Command = ADD | DELETE | UPDATE | GET | SEARCH
    deriving (Show, Eq)
type Metadata = (String, String)
data Result = AddResult String [Metadata]
            | DeleteResult String
            | UpdateResult String String [Metadata]
            | GetResult String
            | SearchResult String Int [Metadata]
    deriving (Show, Eq)

parseQuery :: Parser Result
parseQuery = do
    cmd <- parseCommand
    case cmd of
        ADD -> do
            fileName <- parseFileName
            metadata <- many (try parseMetadata)
            return (AddResult fileName metadata)
        DELETE -> do
            fileId <- parseFileId
            return (DeleteResult fileId)
        UPDATE -> do
            fileId <- parseFileId
            fileName <- parseFileName
            metadata <- many (try parseMetadata)
            return (UpdateResult fileId fileName metadata)
        GET -> do
            fileId <- parseFileId
            return (GetResult fileId)
        SEARCH -> do
            fileName <- parseFileName
            metadata <- many (try parseMetadata)
            countFiles <- parseCount
            return (SearchResult fileName countFiles metadata)

 

parseCommand :: Parser Command
parseCommand = do
    cmd <- choice
        [ ADD <$ string "ADD"
        , DELETE <$ string "DELETE"
        , UPDATE <$ string "UPDATE"
        , GET <$ string "GET"
        , SEARCH <$ string "SEARCH"
        ]
    space
    return cmd

parseFileName :: Parser String
parseFileName = do
    fname <- someTill anySingle (try (space1 *> void (string "metadata:")) <|> eof)
    return fname

parseFileId :: Parser String
parseFileId = do
    _ <- string "id="
    prefix <- some alphaNumChar
    _ <- char '_'
    fileId <- some digitChar
    return (prefix ++ "_" ++ fileId)

parseMetadata :: Parser Metadata
parseMetadata = do
    key <- some alphaNumChar
    _ <- char '='
    value <- manyTill anySingle (void (char ';') <|> lookAhead eof)
    return (key, value)

parseCount :: Parser Int
parseCount = do
    _ <- string "count="
    countNum <- some digitChar
    return (read countNum)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
