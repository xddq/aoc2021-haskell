-- example data
-- forward 5
-- down 5
-- forward 8
-- up 3
-- down 8
-- forward 2
main = do
  input <- readFile "inputDay2"
  putStrLn $
    show $
    ex1 (0, 0) $
    map
      (\line ->
         let (command:value) = words line
          in (command, toInt $ head value)) $
    lines input
  return ()
  where
    toInt xs = read xs :: Int

ex1 :: (Int, Int) -> [(String, Int)] -> Int
ex1 (xPos, yPos) [] = xPos * yPos
ex1 (xPos, yPos) ((command, value):xs)
  | command == "forward" = ex1 (xPos + value, yPos) xs
  | command == "up" = ex1 (xPos, yPos - value) xs
  | otherwise = ex1 (xPos, yPos + value) xs
