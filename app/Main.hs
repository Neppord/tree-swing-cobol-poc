#! /usr/bin/env stack
-- stack --resolver lts-14.3 script


import Data.Maybe

import Cobol
import Cobol.Eval
import Cobol.Parse
import System.Environment (getArgs)
import System.Exit (exitFailure)



main = do
  args <- getArgs
  case args of
    [fileName] -> do
      maybeFile <- parseFromFile fileName
      case maybeFile of
        Just file -> do
          let program = evalScript fileName file
          fromMaybe (putStrLn ("Error interpreting: " <> fileName)) program
        Nothing -> exitFailure
    _ -> putStrLn "Error while parsing program arguments"
