module Lib
  ( euler1
  , euler2
  ) where


-- https://projecteuler.net/problem=1
euler1 :: String
euler1 = show result where
  result = sum [ x | x <- [0..999],
                     any (==0) [ x `mod` 3, x `mod` 5 ] ]

-- https://projecteuler.net/problem=2
euler2 :: String
euler2 = show result where
  result = sum $ filter even $ takeWhile (<4000000) fibs
  fibs = 1 : 2 : zipWith (+) fibs (tail fibs)