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
            _ <- optional space1
            fileName <- parseFileName
            metadata <- many (try parseMetadata)
            return (UpdateResult fileId fileName metadata)
        GET -> do
            fileId <- parseFileId
            return (GetResult fileId)
        SEARCH -> do
            fileName <- parseFileName
            metadata <- many (try parseMetadata)
            _ <- optional space1
            _ <- string "->"
            _ <- optional space1
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
    _ <- optional space1
    fname <- someTill anySingle (try (space1 *> void (string "metadata:")) <|> void (char ';'))
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
    value <- manyTill anySingle (try (void (char ',')) <|> void (char ';'))
    return (key, value)

parseCount :: Parser Int
parseCount = do
    countNum <- some digitChar
    return (read countNum)

