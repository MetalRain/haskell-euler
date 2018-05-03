module Main where

import Lib

results :: [String]
results = [ euler1
          ]

main :: IO ()
main = putStrLn $ foldr (++) "" results