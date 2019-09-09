module Cobol.Ast where

data Value
  = Undefined
  | Str String

data Expr
  = ExprValue Value
  | Id String

data AcceptSource
  = Environment

data Verb
  = Display [Expr]
  | Accept String AcceptSource

newtype Section =
  DefaultSection [Verb]

data Record
  = Record Integer String String Value
--  | Group Integer String [Record]

data File = File
  { workingStorageSection:: [Record]
  , procedureDivision:: [Section]
  }

emptyFile = File
  { workingStorageSection = []
  , procedureDivision = []
  }

newtype Project = Project [(String, File)]

