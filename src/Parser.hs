module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

parseCommand :: Parser String
parseCommand = do
    cmd <- choice
        [ string "ADD"
        , string "DELETE"
        , string "UPDATE"
        , string "GET"
        , string "SEARCH"
        ]
    return cmd

someFunc :: IO ()
someFunc = putStrLn "someFunc"
