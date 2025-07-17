module Main where

import qualified Parser
import Text.Megaparsec
import Data.Void

-- Example test cases for manual testing
testInputs :: [String]
testInputs =
  [ "ADD file1 metadata: key1=val1, key2=val2;"
  , "ADD file2"
  , "DELETE ->doc_123;"
  , "UPDATE ->doc_123 file2 metadata: key=val;"
  , "GET ->doc_123;"
  , "SEARCH ->2 file2 metadata: k1=v1, k2=v2;"
  , "DROP;"
  , "ADD file3 metadata: k=v;"
  , "ADD file4metadata: k=v;"  -- should not produce metadata (missing space before metadata)
  , "ADD file5 metadata: k=v; DELETE ->doc_5; DROP;"
  ]

main :: IO ()
main = mapM_ runTest testInputs
  where
    runTest input = do
      putStrLn $ "\nInput: " ++ show input
      putStrLn $ Parser.runParseAllQueriesAsJson input

