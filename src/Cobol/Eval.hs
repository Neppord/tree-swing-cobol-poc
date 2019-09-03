module Cobol.Eval where

import System.Environment
import Data.Maybe
import Control.Monad.State

import Cobol.Ast

initMemory records = records

-- check that key exists
-- none existing keys should result in different error then empty field
readMemory key [] = Undefined
readMemory key (Record _ name _ value:records)
  | key == name = value
  | otherwise   = readMemory key records

-- check that
--  * key exists
--  * picure and value matches
writeMemory key value' [] = []
writeMemory key value' (Record level name picture value: memmory)
  | key == name  = Record level name picture value': memmory
  | otherwise    = Record level name picture value: writeMemory key value' memmory

evalProject :: String -> Project -> Maybe (IO ())
evalProject filename (Project files) =
  do
    file <- lookup filename files
    return $ evalFile (Project files) file

evalFile ::  Project -> File -> IO ()
evalFile project File {workingStorageSection=records, procedureDivision=[section]} =
  evalStateT program (initMemory records)
  where
    program = evalSection section

evalSection (DefaultSection verbs) =
  mapM_ evalVerb verbs

evalVerb :: Verb -> StateT [Record] IO ()
evalVerb (Accept name Environment) = do
  value <- liftIO $ getEnv name
  modify $ writeMemory name (Str value)
  return ()
evalVerb (Display exprs) = do
  strings <- forM exprs exprToString
  let string = foldl (<>) "" strings :: String
  liftIO $ putStrLn string

exprToString :: Expr -> StateT [Record] IO String
exprToString (ExprValue value) = return $ valueToString value
exprToString (Id str) = gets (valueToString . readMemory str)

valueToString :: Value -> String
valueToString Undefined = ""
valueToString (Str string) = string

