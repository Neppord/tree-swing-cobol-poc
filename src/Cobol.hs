#! /usr/bin/env stack
-- stack --resolver lts-14.3 script

module Cobol where

import Control.Monad.State
import Data.Maybe

import Cobol.Ast
import Cobol.Eval


emptyFile = File
  { workingStorageSection = []
  , procedureDevision = []
  }

example =
  Project
    [ ( "HelloWorld.cbl"
      , ( emptyFile
            { workingStorageSection =
                [ Record 01 "phrase" "A(5)" (Str "Hello")
                , Record 01 "subject" "A(11)" Undefined
                ]
            , procedureDevision =
                [ DefaultSection
                    [ Accept "subject" Environment 
                    , Display 
                        [ Id "phrase"
                        , ExprValue (Str " ")
                        , Id "subject" 
                        ]
                    ]
                ]
            }
        )
      )
    ]
