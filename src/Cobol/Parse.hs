module Cobol.Parse where

import Data.Functor
import Control.Applicative
import Control.Monad.IO.Class

import Text.Trifecta

import Cobol.Ast

parseFromFile :: MonadIO m => String -> m (Maybe File)
parseFromFile = Text.Trifecta.parseFromFile cobolParser

cobolParser :: Parser File
cobolParser =
  skipOptional identificationDivisionParser *>
  (createFile <$> dataDivisionParser <*> procedureDivisionParser)
  <* Text.Trifecta.eof
  where
    createFile :: [Record] -> [Section] -> File
    createFile rs ss = File {
      workingStorageSection = rs,
      procedureDivision = ss
    }
identificationDivisionParser :: Parser ()
identificationDivisionParser =
  string "identification division." <* newline $> () 

dataDivisionParser =
  string "data division."  <* newline
  *> workingStorageSectionParser

workingStorageSectionParser :: Parser [Record]
workingStorageSectionParser =
  string "working-storage section."  <* newline
  *> many recordParser

recordParser :: Parser Record
recordParser =
   Record <$> (read <$> string "01 ") <*> string "msg" <* spaces <*> pictureParser <*> valueParser  <* string "."  <* newline

valueParser :: Parser Value
valueParser =
  (string "value " *> string "\"hello world\"")
  $> Str "hello world"

pictureParser :: Parser String
pictureParser = string "pic " *> string "a(11) "

procedureDivisionParser :: Parser [Section]
procedureDivisionParser =
  string "procedure division."  <* newline
   *> string "display msg."
   $> [Cobol.Ast.DefaultSection [
      Display [Id "msg"]
   ]]