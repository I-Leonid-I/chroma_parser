module MyLib (someFunc) where

import Text.Megaparsec
import Data.Void

type Parser = Parsec Void String

someFunc :: IO ()
someFunc = putStrLn "someFunc"
