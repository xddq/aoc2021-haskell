{-|
Example data:
-}
main
  -- input <- lines <$> readFile "inputDay11"
 = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  -- print $ ex2 input
  return ()

ex1 :: [[Char]] -> Int
ex1 input = 1

ex2 :: [[Char]] -> Int
ex2 input = 2
