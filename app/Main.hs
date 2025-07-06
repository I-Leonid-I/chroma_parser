module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let input = "ADD name of the file metadata:a=b"
  let res :: Either (ParseErrorBundle String Void) Parser.Result
      -- runParser - to run the parser
      -- parser function name
      -- source name (file name)
      -- input string to parse
      res = runParser Parser.parseQuery "" input
  case res of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right value -> putStrLn $ "Parsed: " ++ show value
