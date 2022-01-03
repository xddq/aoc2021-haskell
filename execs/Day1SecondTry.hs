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
  print $ ex1 $ map toInt $ lines input
  print $
    ex1 $
    -- Creates list of the sum of 3 following values. [199 + 200 + 208, 200 +
    -- 208 + 210, etc... ]
    zipWith3
      (\x y z -> toInt x + toInt y + toInt z)
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
