-- example data
-- 199
-- 200
-- 208
-- 210
-- 200
-- 207
-- 240
-- 269
-- 260
-- 263
main = do
  input <- readFile "inputDay1"
  putStrLn $ show $ ex1 $ map toInt $ lines input
  putStrLn $
    show $
    ex2 $
    map (\threeVals -> map toInt threeVals) $
    -- Creates list of list with each inner list containing three values in a
    -- row. [[199,200,208],[200,208,210], etc..]
    zipWith3
      (\x y z -> x : y : [z])
      (lines input)
      (drop 1 $ lines input)
      (drop 2 $ lines input)
  return ()
  where
    toInt xs = read xs :: Int

ex1 :: [Int] -> Int
ex1 [_] = 0
ex1 (x1:x2:xs)
  | x2 > x1 = 1 + ex1 (x2 : xs)
  | otherwise = ex1 (x2 : xs)

ex2 :: [[Int]] -> Int
ex2 [_] = 0
ex2 (x1:x2:xs)
  | sum x2 > sum x1 = 1 + ex2 (x2 : xs)
  | otherwise = ex2 (x2 : xs)
