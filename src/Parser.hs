{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

type Parser = Parsec Void String

data Command = ADD | DELETE | UPDATE | GET | SEARCH | DROP
    deriving (Show, Eq)
type Metadata = (String, String)
data Result = AddResult String [Metadata]
            | DeleteResult String
            | UpdateResult String String [Metadata]
            | GetResult String
            | SearchResult String Int [Metadata]
            | DropResult
    deriving (Show, Eq)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

spaceReq :: String -> Parser String
spaceReq s = string s <* space1

parseAllQueries :: Parser [Result]
parseAllQueries = some (try (parseQuery <* space)) <* eof

runParseAllQueries :: String -> [Result]
runParseAllQueries input =
    case runParser parseAllQueries "" input of
        Left err -> handleError err
        Right results -> results


handleError :: ParseErrorBundle String Void -> [Result]
handleError err = 
    error ("Hi, here is a mistake(((( Custom parse error: " ++ errorBundlePretty err)


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
            _ <- char ';'
            return (GetResult fileId)
        SEARCH -> do
            countFiles <- parseCount
            fileName <- parseFileName
            metadata <- many (try parseMetadata)
            return (SearchResult fileName countFiles metadata)
        DROP -> do
            return DropResult


parseCommand :: Parser Command
parseCommand = choice
    [ ADD <$ symbol "ADD"
    , DELETE <$ symbol "DELETE"
    , UPDATE <$ symbol "UPDATE"
    , GET <$ symbol "GET"
    , SEARCH <$ symbol "SEARCH"
    , DROP <$ symbol "DROP"
    ]


parseFileName :: Parser String
parseFileName = fmap (dropWhileEnd isSpace) $
    lexeme $ someTill anySingle (try (symbol "metadata:") <|> symbol ";")

parseFileId :: Parser String
parseFileId = do
    _ <- symbol "->"
    prefix <- lexeme $ some alphaNumChar
    _ <- char '_'
    fileId <- some digitChar
    return (prefix ++ "_" ++ fileId)

parseMetadata :: Parser Metadata
parseMetadata = do
    key <- lexeme $ some alphaNumChar
    _ <- symbol "="
    value <- lexeme $ manyTill anySingle (try (void (char ',')) <|> void (char ';'))
    return (key, value)

parseCount :: Parser Int
parseCount = do
    _ <- symbol "->"
    countNum <- lexeme $ some digitChar
    return (read countNum)

