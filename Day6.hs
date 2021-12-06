-- example data
--
-- for spliOn
import Data.List.Split

-- respawn timer 7 days. (6,5,.. 0)
-- new fish -> +2 days -> 9 days  (8,7,..0)
-- after each day, 0 -> 6 and spawns an 8, rest decreases
main = do
  input <- readFile "inputDay6"
  let lanternfish = map toInt $ splitOn "," input
  let days = 80
  let moreDays = 256
  print $ ex1 lanternfish 256
  -- print $ ex2 lanternfish moreDays
  return ()

ex1 :: [Int] -> Int -> Int
ex1 ages 0 = length ages
ex1 ages days =
  let newAges =
        concatMap
          (\age ->
             if age == 0
               then [8, 6]
               else [age - 1])
          ages
   in ex1 newAges (days - 1)

toInt :: String -> Int
toInt = read
