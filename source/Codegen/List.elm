module Codegen.List exposing (..)

import Utils exposing (flip)


list : List String -> String
list =
    String.join "\n  , "
        >> (++) "[ "
        >> flip (++) " ]"
