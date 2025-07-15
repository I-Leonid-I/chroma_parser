module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let input = "ADD doc 1234"
  let results = Parser.runParseAllQueries input
  mapM_ print results
