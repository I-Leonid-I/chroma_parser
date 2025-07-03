module MyLib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

parserString :: Parser String
parserString = do
    a <- string "Hello"
    return a

someFunc :: IO ()
someFunc = putStrLn "someFunc"
