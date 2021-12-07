-- example data
-- Initial state: 3,4,3,1,2
-- After  1 day:  2,3,2,0,1
-- After  2 days: 1,2,1,6,0,8
-- After  3 days: 0,1,0,5,6,7,8
-- for spliOn
import Data.List.Index

-- for modifyAt
import Data.List.Split

-- for Maps
import Data.Map.Strict as M
import Prelude as P

-- respawn timer 7 days. (6,5,.. 0)
-- new fish -> +2 days -> 9 days  (8,7,..0)
-- after each day, 0 -> 6 and spawns an 8, rest decreases
main = do
  input <- readFile "inputDay6"
  let lanternfish = P.map toInt $ splitOn "," input
  let days = 80
  print $ ex1 lanternfish days
  -- Need to adapt how we store/count the fish for ex2 since numbers get to
  -- big/lists to large.
  let moreDays = 256
  let parsedFish =
        parseFish lanternfish $ fromList $ P.map (\val -> (val, 0)) [0 .. 8]
  print $ ex2 parsedFish moreDays
  return ()

ex2 :: [Int] -> Int -> Int
ex2 fishs 0 = sum fishs
ex2 fishs days =
  let amount = head fishs
      agedFishs = tail fishs ++ [0]
      spawnNewFishs = modifyAt 8 (+ amount) $ modifyAt 6 (+ amount) agedFishs
   in ex2 spawnNewFishs (days - 1)

-- Counts fish for given ages and creates a list where the index is the given
-- age of the fish and the value at the index is the given amount of fish.
-- Requires a map with keys 0-9 and value 0 for each as helper input!
parseFish :: [Int] -> Map Int Int -> [Int]
parseFish [] kvs = reverse $ M.foldl (\acc val -> val : acc) [] kvs
parseFish (x:xs) kvs = parseFish xs $ M.insertWith (+) x 1 kvs

ex1 :: [Int] -> Int -> Int
ex1 ages 0 = length ages
ex1 ages days =
  let newAges =
        P.foldl
          (\ages age ->
             if age == 0
               then 8 : 6 : ages
               else age - 1 : ages)
          []
          ages
   in ex1 newAges (days - 1)

toInt :: String -> Int
toInt = read
