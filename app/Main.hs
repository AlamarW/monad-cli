module Main (main) where

import System.Environment (getArgs)
import Data.Text (pack)

-- | Main entry point for monad-cli
-- Accepts pipeline expressions as command-line arguments
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: monad-cli '<pipeline-expression>'"
    (expr:_) -> do
      putStrLn $ "Received pipeline expression: " ++ expr
      putStrLn "Pipeline execution not yet implemented"
      -- TODO: Parse and execute pipeline
      -- let pipelineExpr = pack expr
      -- case parsePipeline pipelineExpr of
      --   Left err -> putStrLn $ "Parse error: " ++ err
      --   Right pipeline -> executePipeline pipeline
