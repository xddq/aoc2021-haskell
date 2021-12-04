-- example data
-- 00100
-- 11110
-- 10110
-- 10111
-- 10101
-- 01111
-- 00111
-- 11100
-- 10000
-- 11001
-- 00010
-- 01010
-- used for transpose
import Data.List

main = do
  input <- readFile "inputDay3"
  -- Creates list of lists containing the given binary values as ints. E.g.
  -- [[0,0,1,0,0],[1,1,1,1,0], etc..]
  let inputRows = map split $ lines input
      -- flips list of rows into list of cols
      inputCols = transpose inputRows
  print $ ex1 inputCols
  print $ ex2 inputRows
  return ()

ex1 :: [[Int]] -> Int
ex1 input =
  (\(gamma, epsilon) -> gamma * epsilon) $
  calcGammaAndEpsilon $
  map
    (\(zeros, ones) ->
       if zeros > ones
         then 0
         else 1) $
  map countZerosAndOnes input
  where
    calcGammaAndEpsilon xs = (calcBin xs, calcBin $ map invertBin xs)

-- takes string that represents a binary and returns a list containing each
-- value. split "0101" = [0,1,0,1]
split :: String -> [Int]
split [] = []
split (x:xs) = read [x] : split xs

ex2 input =
  calcRating input 0 bitCriteriaCO2 * calcRating input 0 bitCriteriaOxygen

calcRating rows pos criteria =
  let (zeros, ones) = countZerosAndOnes $ (transpose rows) !! pos
      resultingBit = criteria zeros ones
      filteredRows = filter (\row -> (row !! pos) == resultingBit) rows
   in if length rows == 1
        then calcBin $ head rows
        else calcRating filteredRows (pos + 1) criteria

-- picks bigger values, and one if eq.
bitCriteriaOxygen zeros ones =
  case compare zeros ones of
    GT -> 0
    EQ -> 1
    LT -> 1

-- picks smaller values, and zero if eq.
bitCriteriaCO2 zeros ones =
  case compare zeros ones of
    GT -> 1
    EQ -> 0
    LT -> 0

invertBin x =
  if x == 1
    then 0
    else 1

countZerosAndOnes =
  foldl
    (\(zeros, ones) x ->
       if x == 1
         then (zeros, ones + 1)
         else (zeros + 1, ones))
    (0, 0)

calcBin :: [Int] -> Int
calcBin xs = calcBin' (reverse xs) 0

calcBin' :: [Int] -> Int -> Int
calcBin' [] _ = 0
calcBin' (x:xs) y
  -- power in haskell: https://wiki.haskell.org/Power_function
  | x == 1 = 2 ^ y * x + calcBin' xs (y + 1)
  | otherwise = calcBin' xs (y + 1)
