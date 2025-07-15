module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let input = "ADD file1 metadata: 1=2;"
  let results = Parser.runParseAllQueries input
  mapM_ print results
