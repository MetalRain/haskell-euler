module Lib ( results ) where

import Data.List (sort, find, tails)
import Data.Maybe (fromMaybe, isJust)

results :: [String]
results = [ euler1
          , euler2
          , euler3
          , euler4
          , euler5
          , euler6
          , euler7
          , euler8
          , euler9
          ]

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


-- https://projecteuler.net/problem=6
euler6 :: String
euler6 = show result where
  result = sqOfSum - sumOfSq
  range = [1..100]
  sqOfSum = (sum range) ^ 2
  sumOfSq = sum $ map (\x -> x ^ 2) range


-- https://projecteuler.net/problem=7
euler7 :: String
euler7 = show result where
  result = primes !! 10000


-- https://projecteuler.net/problem=8
euler8Num = "73167176531330624919225119674426574742355349194934\
            \96983520312774506326239578318016984801869478851843\
            \85861560789112949495459501737958331952853208805511\
            \12540698747158523863050715693290963295227443043557\
            \66896648950445244523161731856403098711121722383113\
            \62229893423380308135336276614282806444486645238749\
            \30358907296290491560440772390713810515859307960866\
            \70172427121883998797908792274921901699720888093776\
            \65727333001053367881220235421809751254540594752243\
            \52584907711670556013604839586446706324415722155397\
            \53697817977846174064955149290862569321978468622482\
            \83972241375657056057490261407972968652414535100474\
            \82166370484403199890008895243450658541227588666881\
            \16427171479924442928230863465674813919123162824586\
            \17866458359124566529476545682848912883142607690042\
            \24219022671055626321111109370544217506941658960408\
            \07198403850962455444362981230987879927244284909188\
            \84580156166097919133875499200524063689912560717606\
            \05886116467109405077541002256983155200055935729725\
            \71636269561882670428252483600823257530420752963450"

slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow n xs = map (take n) (tails xs)

euler8 :: String
euler8 = show result where
  result = foldr max 0 sums
  sums = map (\xs -> foldr (*) 1 xs) slices
  slices = slidingWindow 13 numbers
  numbers = (map (\c -> read [c] :: Int) euler8Num)


-- https://projecteuler.net/problem=9
euler9 :: String
euler9 = show result where
  result = head [ a * b * c | a <- [1..1000 - 2]
                            , b <- [1..1000 - a - 1]
                            , let c = 1000 - a - b
                            , (a ^ 2) + (b ^ 2) == c ^ 2
                            ]