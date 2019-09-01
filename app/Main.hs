#! /usr/bin/env stack
-- stack --resolver lts-14.3 script

import Cobol
import Cobol.Eval
import Data.Maybe

main = fromMaybe (return ()) program
  where
    program = evalProject "HelloWorld.cbl" example
