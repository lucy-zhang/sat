module Main where

import Sat

main :: IO ()
main = interact (\s ->
    case parseCnf s of
        Left err -> show err
        Right (nVars, inst) -> (formatAssignment . solve nVars . cnfToExpr) inst <> "\n")
