{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char (isSpace)
import Control.Monad (void, when)
import Data.List (dropWhileEnd)

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

spaceReq :: Parser a -> Parser a
spaceReq p = space1 *> p

lexemeLeaveOneSpace :: Parser a -> Parser a
lexemeLeaveOneSpace p = do
    x <- p
    _ <- manyTill spaceChar (lookAhead spaceChar)
    return x


parseAllQueries :: Parser [Result]
parseAllQueries = many parseQuery <* eof

runParseAllQueries :: String -> [Result]
runParseAllQueries input =
    case runParser parseAllQueries "" input of
        Left err -> handleError err
        Right results -> results


handleError :: ParseErrorBundle String Void -> [Result]
handleError err = error (errorBundlePretty err)


parseQuery :: Parser Result
parseQuery = choice
    [ parseAdd
    , parseDelete
    , parseUpdate
    , parseGet
    , parseSearch
    , parseDrop
    ]

parseAdd :: Parser Result
parseAdd = do
    _ <- string "ADD" <?> "'ADD' command"
    fileName <- spaceReq parseFileName
    metadata <- many (try parseMetadata)
    _ <- symbol ";" <?> "';' at the end of ADD command"
    return (AddResult fileName metadata)

parseDelete :: Parser Result
parseDelete = do
    _ <- symbol "DELETE" <?> "'DELETE' command"
    fileId <- parseFileId
    _ <- symbol ";" <?> "';' at the end of DELETE command"
    return (DeleteResult fileId)

parseUpdate :: Parser Result
parseUpdate = do
    _ <- symbol "UPDATE" <?> "'UPDATE' command"
    fileId <- parseFileId
    fileName <- spaceReq parseFileName
    metadata <- many (try parseMetadata)
    _ <- symbol ";" <?> "';' at the end of UPDATE command"
    return (UpdateResult fileId fileName metadata)

parseGet :: Parser Result
parseGet = do
    _ <- symbol "GET" <?> "'GET' command"
    fileId <- parseFileId
    _ <- symbol ";" <?> "';' at the end of GET command"
    return (GetResult fileId)

parseSearch :: Parser Result
parseSearch = do
    _ <- symbol "SEARCH" <?> "'SEARCH' command"
    countFiles <- parseCount
    fileName <- spaceReq parseFileName
    metadata <- many (try parseMetadata)
    _ <- symbol ";" <?> "';' at the end of SEARCH command"
    return (SearchResult fileName countFiles metadata)

parseDrop :: Parser Result
parseDrop = do
    _ <- symbol "DROP" <?> "'DROP' command"
    _ <- symbol ";" <?> "';' at the end of DROP command"
    return DropResult


parseFileName :: Parser String
parseFileName = lexeme $ do
    fileName <- manyTill anySingle (try (lookAhead (void (string "metadata:"))) <|> try (lookAhead (void (char ';'))))
    return (dropWhileEnd isSpace fileName)

parseFileId :: Parser String
parseFileId = do
    _ <- symbol "->" <?> "'->' before file ID"
    prefix <- lexeme $ some alphaNumChar
    when (prefix /= "doc") $ fail "File ID must start with 'doc'"
    _ <- char '_' <?> "'_' after prefix 'doc' in file ID"
    fileId <- some digitChar
    return (prefix ++ "_" ++ fileId)

parseMetadata :: Parser Metadata
parseMetadata = do
    _ <- symbol "metadata:" <?> "'metadata:' keyword"
    key <- lexeme (some alphaNumChar <?> "metadata key")
    _ <- symbol "=" <?> "'=' after metadata key"
    value <- lexeme (manyTill anySingle (try (void (char ',')) <|> try (lookAhead (void (char ';'))))) <?> "metadata value followed by ',' or ';'"
    return (key, value)

parseCount :: Parser Int
parseCount = do
    _ <- symbol "->" <?> "'->' before number of files"
    countNum <- lexeme $ some digitChar
    return (read countNum)