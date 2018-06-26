module Main where

import Lib

concatResults :: [(Int, String)] -> String
concatResults rs = foldr (++) "" lines where
  lines = map (\(i, r) -> "Euler " ++ show i ++ ": " ++ r ++ "\n") rs

main :: IO ()
main = putStrLn $ concatResults $ zip [1..] results