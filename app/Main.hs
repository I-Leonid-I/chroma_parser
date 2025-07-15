module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

main :: IO ()
main = do
  let input = "ADD file1 metadata:hahah=4; DELETE ->id_1;DROP Y"
  let results = Parser.runParseAllQueries input
  mapM_ print results
