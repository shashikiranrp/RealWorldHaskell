module Main where

import SimpleJSON
import PutJSON

main :: IO ()
main = putStrLn "Enter name and age in two defferent lines: " >>
       sequence [getLine, getLine] >>=
       (\ls -> putJValue $ JObject [("name", JString $ head ls), ("Age", JNumber ((read . last $ ls) :: Double))])
