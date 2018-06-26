module Main where

import Lib

results :: [String]
results = [ euler1
          , euler2
          , euler3
          , euler4
          , euler5
          , euler6
          ]

main :: IO ()
main = putStrLn $ foldr (\acc res -> acc ++ "\n" ++ res) "" results