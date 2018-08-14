module Main where

import Sat

main :: IO ()
main = interact (\s ->
    case parseCnf s of
        Left err -> show err
        Right (_, inst) -> (formatAssignment . solve . transform) inst <> "\n")
