module Lib
  ( euler1
  ) where

-- https://projecteuler.net/problem=1
euler1 :: String
euler1 = show result where
  result = foldr (+) 0 [ x | x <- [0..999],
                             any (==0) [ x `mod` 3, x `mod` 5 ] ]