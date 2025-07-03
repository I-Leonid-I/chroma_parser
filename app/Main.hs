module Main where

import qualified MyLib
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let input = "Hello World"
  let res :: Either (ParseErrorBundle String Void) String
      -- runParser - to run the parser
      -- parser function name
      -- source name (file name)
      -- input string to parse
      res = runParser MyLib.parserString "" input
  case res of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right value -> putStrLn $ "Parsed: " ++ value
