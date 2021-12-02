-- example data
-- forward 5
-- down 5
-- forward 8
-- up 3
-- down 8
-- forward 2
main = do
  input <- readFile "inputDay2"
  let preparedInput = prepareInput input
  print $ ex1 (0, 0) preparedInput
  print $ ex2 (0, 0, 0) preparedInput
  return ()

-- Creates a list of string value tuples. [(forward, 5), (down, 5), etc...]
prepareInput :: String -> [(String, Int)]
prepareInput input =
  map
    (\line ->
       let (command:value) = words line
        in (command, toInt $ head value)) $
  lines input
  where
    toInt xs = read xs :: Int

-- Adapts the current X and Y positions of the submarine depending on the given
-- command. Multiplies resulting X and Y positions once we iterated through all
-- commands.
ex1 :: (Int, Int) -> [(String, Int)] -> Int
ex1 (xPos, yPos) [] = xPos * yPos
ex1 (xPos, yPos) ((command, value):xs)
  | command == "forward" = ex1 (xPos + value, yPos) xs
  | command == "up" = ex1 (xPos, yPos - value) xs
  | otherwise = ex1 (xPos, yPos + value) xs

-- Adapted semantic of up, down and foward based on description. Added aim as
-- thrid parameter to keep track of.
ex2 :: (Int, Int, Int) -> [(String, Int)] -> Int
ex2 (xPos, yPos, aim) [] = xPos * yPos
ex2 (xPos, yPos, aim) ((command, value):xs)
  | command == "forward" = ex2 (xPos + value, yPos + aim * value, aim) xs
  | command == "up" = ex2 (xPos, yPos, aim - value) xs
  | otherwise = ex2 (xPos, yPos, aim + value) xs
