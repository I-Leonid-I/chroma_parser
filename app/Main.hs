module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let input = "AD doc 1234 metadata: a=b;"
  let results = Parser.runParseAllQueries input
  mapM_ print results
