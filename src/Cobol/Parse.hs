module Cobol.Parse where

import Control.Applicative
import Text.Trifecta

import Cobol.Ast

cobolParser :: Parser File
cobolParser =
  skipOptional identificationDivisionParser *>
  (createFile <$> dataDivisionParser <*> procedureDivisionParser)
  where
    createFile :: [Record] -> [Section] -> File
    createFile rs ss = File {
      workingStorageSection = rs,
      procedureDivision = ss
    }

identificationDivisionParser =
  string "identification division."

dataDivisionParser =
  string "data division."
  *> workingStorageSectionParser

workingStorageSectionParser :: Parser [Record]
workingStorageSectionParser =
  string "working-storage section."
  *> return ([])

procedureDivisionParser :: Parser [Section]
procedureDivisionParser =
  string "procedure division."
  *> return ([Cobol.Ast.DefaultSection []])