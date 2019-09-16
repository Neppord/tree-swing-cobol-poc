module Cobol.Ast where

data Value
  = Undefined
  | Str String
  deriving Show

data Expr
  = ExprValue Value
  | Id String
  deriving Show

data AcceptSource
  = Environment
  deriving Show

data Verb
  = Display [Expr]
  | Accept String AcceptSource
  deriving Show

newtype Section =
  DefaultSection [Verb]
  deriving Show

data Record
  = Record Integer String String Value
--  | Group Integer String [Record]
  deriving Show

data File = File
  { workingStorageSection:: [Record]
  , procedureDivision:: [Section]
  }
  deriving Show

emptyFile = File
  { workingStorageSection = []
  , procedureDivision = []
  }

newtype Project = Project [(String, File)]
  deriving Show

script name file = Project [(name, file)] 

