module Lib
  ( euler1
  , euler2
  , euler3
  , euler4
  , euler5
  ) where

import Data.List (sort, find)
import Data.Maybe (fromMaybe, isJust)

-- https://projecteuler.net/problem=1
euler1 :: String
euler1 = show result where
  result = sum [ x | x <- [0..999],
                     any (==0) [ x `mod` 3, x `mod` 5 ] ]


-- https://projecteuler.net/problem=2
fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

euler2 :: String
euler2 = show result where
  result = sum $ filter even $ takeWhile (<4000000) fibs


-- https://projecteuler.net/problem=3
primes :: [Int]
primes = filterPrime [2..] where
  filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

factorsOf :: Int -> [Int]
factorsOf x = factors where
  factor = head $ filter (\p -> x `mod` p == 0) primes
  factors = case (x `div` factor) of
    1   -> [factor]
    rem -> factor : factorsOf rem

euler3 :: String
euler3 = show result where
  result = head $ reverse $ factorsOf 600851475143



-- https://projecteuler.net/problem=4
isPalindrome :: String -> Bool
isPalindrome s = reverse s == s

euler4 :: String
euler4 = show result where
  range = reverse [900..999]
  result = head [ x * y | x <- range
                        , y <- range
                        , isPalindrome $ show (x * y) ]


-- https://projecteuler.net/problem=5
isEvenlyDivisibleUpTo :: Int -> Int -> Bool
isEvenlyDivisibleUpTo n x = not $ any (\a -> x `mod` a /= 0) [1..n]

euler5 :: String
euler5 = show result where
  result = head $ filter (isEvenlyDivisibleUpTo 20) [19*18..]