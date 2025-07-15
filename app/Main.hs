module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let input = "DROP; ADD name of the file;"
  let results = Parser.runParseAllQueries input
  mapM_ print results
