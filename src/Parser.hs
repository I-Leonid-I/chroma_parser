
module Parser where


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char (isSpace)
import Control.Monad (void, when)
import Data.List (dropWhileEnd, find, isPrefixOf)


type Parser = Parsec Void String


data Command = ADD | DELETE | UPDATE | GET | SEARCH | DROP
    deriving (Show, Eq)
type Metadata = (String, String)
data Query = AddQuery String [Metadata]
            | DeleteQuery String
            | UpdateQuery String String [Metadata]
            | GetQuery String
            | SearchQuery String Int [Metadata]
            | DropQuery
    deriving (Show, Eq)


-- Consumes whitespace in the parser.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- Wraps a parser to consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Parses a given symbol and consumes trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Parses required whitespace before another parser.
spaceReq :: Parser a -> Parser a
spaceReq p = space1 *> p

-- Parses and leaves one space after the parser.
lexemeLeaveOneSpace :: Parser a -> Parser a
lexemeLeaveOneSpace p = do
    x <- p
    _ <- manyTill spaceChar (lookAhead spaceChar)
    return x


-- Parses multiple queries from input until EOF.
parseAllQueries :: Parser [Query]
parseAllQueries = many parseQuery <* eof

-- Runs the parser on input and returns results or throws error.
runParseAllQueries :: String -> [Query]
runParseAllQueries input =
    case runParser parseAllQueries "" input of
        Left err -> handleError err
        Right results -> results

-- Runs the parser and returns either error info or results.
runParseAllQueriesWithError :: String -> Either (String, String) [Query]
runParseAllQueriesWithError input =
    case runParser parseAllQueries "" input of
        Left err -> Left (parseErrorParts err)
        Right results -> Right results

-- Runs the parser and returns results or error as a JSON string.
runParseAllQueriesAsJson :: String -> String
runParseAllQueriesAsJson input =
    case runParser parseAllQueries "" input of
        Left err -> errorToJson err
        Right results -> parseMultipleToJson results



-- Throws a formatted parse error.
handleError :: ParseErrorBundle String Void -> [Query]
handleError err = error (errorBundlePretty err)


-- Extracts unexpected and expecting parts from a parse error.
parseErrorParts :: ParseErrorBundle String Void -> (String, String)
parseErrorParts err = 
    let errorMsg = errorBundlePretty err
        errorLines = lines errorMsg
        unexpectedPart = extractUnexpected errorLines
        expectingPart = extractExpecting errorLines
    in (unexpectedPart, expectingPart)

-- Extracts the unexpected part from error lines.
extractUnexpected :: [String] -> String
extractUnexpected errorLines = 
    case find ("unexpected " `isPrefixOf`) errorLines of
        Just line -> line
        Nothing -> "unknown"

-- Extracts the expecting part from error lines.
extractExpecting :: [String] -> String
extractExpecting errorLines = 
    case find ("expecting " `isPrefixOf`) errorLines of
        Just line -> line
        Nothing -> "unknown"

-- Converts a parse error to a JSON string.
errorToJson :: ParseErrorBundle String Void -> String
errorToJson err = 
    let (unexpected, expecting) = parseErrorParts err
    in "{\"error\":\"" ++ escapeJson unexpected ++ " -> " ++ escapeJson expecting ++ "\"}"


-- Parses a single query command.
parseQuery :: Parser Query
parseQuery = choice
    [ parseAdd
    , parseDelete
    , parseUpdate
    , parseGet
    , parseSearch
    , parseDrop
    ]


-- Parses an ADD query.
parseAdd :: Parser Query
parseAdd = do
    _ <- string "ADD" <?> "'ADD' command"
    fileName <- spaceReq parseFileName
    metadata <- option [] (try parseMetadata)
    _ <- symbol ";" <?> "';' at the end of ADD command"
    return (AddQuery fileName metadata)


-- Parses a DELETE query.
parseDelete :: Parser Query
parseDelete = do
    _ <- symbol "DELETE" <?> "'DELETE' command"
    fileId <- parseFileId
    _ <- symbol ";" <?> "';' at the end of DELETE command"
    return (DeleteQuery fileId)


-- Parses an UPDATE query.
parseUpdate :: Parser Query
parseUpdate = do
    _ <- symbol "UPDATE" <?> "'UPDATE' command"
    fileId <- parseFileId
    fileName <- spaceReq parseFileName
    metadata <- option [] (try parseMetadata)
    _ <- symbol ";" <?> "';' at the end of UPDATE command"
    return (UpdateQuery fileId fileName metadata)


-- Parses a GET query.
parseGet :: Parser Query
parseGet = do
    _ <- symbol "GET" <?> "'GET' command"
    fileId <- parseFileId
    _ <- symbol ";" <?> "';' at the end of GET command"
    return (GetQuery fileId)


-- Parses a SEARCH query.
parseSearch :: Parser Query
parseSearch = do
    _ <- symbol "SEARCH" <?> "'SEARCH' command"
    countFiles <- parseCount
    fileName <- spaceReq parseFileName
    metadata <- option [] (try parseMetadata)
    _ <- symbol ";" <?> "';' at the end of SEARCH command"
    return (SearchQuery fileName countFiles metadata)


-- Parses a DROP query.
parseDrop :: Parser Query
parseDrop = do
    _ <- symbol "DROP" <?> "'DROP' command"
    _ <- symbol ";" <?> "';' at the end of DROP command"
    return DropQuery



-- Parses a file name.
parseFileName :: Parser String
parseFileName = lexeme $ do
    fileName <- manyTill anySingle (try (lookAhead (void (space1 *> string "metadata:"))) <|> try (lookAhead (void (char ';'))))
    return (dropWhileEnd isSpace fileName)


-- Parses a file ID.
parseFileId :: Parser String
parseFileId = do
    _ <- symbol "->" <?> "'->' before file ID"
    prefix <- lexeme $ some alphaNumChar
    when (prefix /= "doc") $ fail "File ID must start with 'doc'"
    _ <- char '_' <?> "'_' after prefix 'doc' in file ID"
    fileId <- some digitChar
    return (prefix ++ "_" ++ fileId)


-- Parses metadata key-value pairs.
parseMetadata :: Parser [Metadata]
parseMetadata = do
    _ <- symbol "metadata:" <?> "'metadata:' keyword"
    parseMetadataList


-- Parses a list of metadata pairs.
parseMetadataList :: Parser [Metadata]
parseMetadataList = sepBy1 parseKeyValue (symbol ",")


-- Parses a single metadata key-value pair.
parseKeyValue :: Parser Metadata
parseKeyValue = do
    key <- lexeme (some alphaNumChar <?> "metadata key")
    _ <- symbol "=" <?> "'=' after metadata key"
    value <- lexeme (manyTill anySingle (try (lookAhead (void (char ','))) <|> try (lookAhead (void (char ';'))))) <?> "metadata value"
    return (key, value)


-- Parses a count value.
parseCount :: Parser Int
parseCount = do
    _ <- symbol "->" <?> "'->' before number of files"
    countNum <- lexemeLeaveOneSpace $ some digitChar
    return (read countNum)



-- Escapes special characters for JSON output.
escapeJson :: String -> String
escapeJson = concatMap esc
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = [c]


-- Converts a list of queries to a JSON array string.
parseMultipleToJson :: [Query] -> String
parseMultipleToJson results =
    "[" ++ concat (intersperse "," (map resultToJson results)) ++ "]"


-- Converts a single query result to a JSON object string.
resultToJson :: Query -> String
resultToJson (AddQuery fileName metadata) =
    "{" ++
    "\"type\":\"ADD\"," ++
    "\"fileName\":\"" ++ escapeJson fileName ++ "\"," ++
    "\"metadata\": " ++ metadataListToJson metadata ++ "}"
resultToJson (DeleteQuery fileId) =
    "{" ++
    "\"type\":\"DELETE\"," ++
    "\"fileId\":\"" ++ escapeJson fileId ++ "\"}"
resultToJson (UpdateQuery fileId fileName metadata) =
    "{" ++
    "\"type\":\"UPDATE\"," ++
    "\"fileId\":\"" ++ escapeJson fileId ++ "\"," ++
    "\"fileName\":\"" ++ escapeJson fileName ++ "\"," ++
    "\"metadata\": " ++ metadataListToJson metadata ++ "}"
resultToJson (GetQuery fileId) =
    "{" ++
    "\"type\":\"GET\"," ++
    "\"fileId\":\"" ++ escapeJson fileId ++ "\"}"
resultToJson (SearchQuery fileName count metadata) =
    "{" ++
    "\"type\":\"SEARCH\"," ++
    "\"fileName\":\"" ++ escapeJson fileName ++ "\"," ++
    "\"count\": " ++ show count ++ "," ++
    "\"metadata\": " ++ metadataListToJson metadata ++ "}"
resultToJson DropQuery =
    "{" ++
    "\"type\":\"DROP\"" ++ "}"


-- Converts a list of metadata to a JSON object string.
metadataListToJson :: [Metadata] -> String
metadataListToJson [] = "{}"
metadataListToJson ms = "{" ++ (concat $ intersperse "," (map pairToJson ms)) ++ "}"


-- Converts a metadata key-value pair to a JSON string.
pairToJson :: Metadata -> String
pairToJson (k, v) = "\"" ++ escapeJson k ++ "\":\"" ++ escapeJson v ++ "\""


-- Inserts a separator between elements of a list.
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs