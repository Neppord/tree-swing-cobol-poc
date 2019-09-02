module Cobol.Eval where

import System.Environment
import Data.Maybe
import Control.Monad.State

import Cobol.Ast

initMemmory records = records

-- check that key exists
-- none existing keys should result in different error then empty field
readMemmory key [] = Undefined
readMemmory key (Record _ name _ value:records)
  | key == name = value
  | otherwise   = readMemmory key records

-- check that
--  * key exists
--  * picure and value matches
writeMemmory key value' [] = []
writeMemmory key value' (Record level name picture value: memmory)
  | key == name  = Record level name picture value': memmory
  | otherwise    = Record level name picture value: writeMemmory key value' memmory

evalProject :: String -> Project -> Maybe (IO ())
evalProject filename (Project files) =
  do
    file <- lookup filename files
    return $ evalFile (Project files) file

evalFile ::  Project -> File -> IO ()
evalFile project File {workingStorageSection=records, procedureDevision=[section]} =
  evalStateT program (initMemmory records)
  where
    program = evalSection section

evalSection (DefaultSection verbs) =
  mapM_ evalVerb verbs

evalVerb :: Verb -> StateT [Record] IO ()
evalVerb (Accept name Environment) = do
  value <- liftIO $ getEnv name
  modify $ writeMemmory name (Str value)
  return ()
evalVerb (Display exprs) = do
  strings <- forM exprs exprToString
  let string = foldl (<>) "" strings :: String
  liftIO $ putStrLn string

exprToString :: Expr -> StateT [Record] IO String
exprToString (ExprValue value) = return $ valueToString value
exprToString (Id str) = gets (valueToString . readMemmory str)

valueToString :: Value -> String
valueToString Undefined = ""
valueToString (Str string) = string

