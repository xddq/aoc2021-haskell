-- example data
-- crab positions
-- 16,1,2,0,4,2,7,1,2,14
-- possible move sequence
-- Move from 16 to 2: 14 fuel
-- Move from 1 to 2: 1 fuel
-- Move from 2 to 2: 0 fuel
-- Move from 0 to 2: 2 fuel
-- Move from 4 to 2: 2 fuel
-- Move from 2 to 2: 0 fuel
-- Move from 7 to 2: 5 fuel
-- Move from 1 to 2: 1 fuel
-- Move from 2 to 2: 0 fuel
-- Move from 14 to 2: 12 fuel
-- This costs a total of 37 fuel. This is the cheapest possible outcome; more
-- expensive outcomes include aligning at position 1 (41 fuel), position 3 (39
-- fuel), or position 10 (71 fuel).
-- for using Map. src for notation:
-- https://stackoverflow.com/questions/20576229/an-example-of-using-data-map-in-haskell
import Data.Map (Map)
import qualified Data.Map as M

-- for splitOn
import Data.List.Split

{-|
Initial Plan:
    1) create Map with each key being the position and value being the amount of
    crabs at this position.
    2) create a list of all keys/positions inside our map.
    3) iterate/map through list of keys(positions) and calculate the total sum of
    all other crabs (keys and their amount) moving to that position.
    4) grab the lowest result
-}
main = do
  input <- readFile "inputDay7"
  -- let crabPositions = map toInt $ splitOn "," input
  let crabPositions = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]
  let crabMap =
        foldl
          (\acc crabPosition -> M.insertWith (+) crabPosition 1 acc)
          M.empty
          crabPositions
  let positions = M.keys crabMap
  -- print positions
  print $ ex1 crabMap positions Part1
  print $ ex1 crabMap positions Part2
  return ()

type CrabMap = Map Int Int

type CrabPosition = Int

type FuelCost = Int

type StepCost = Int

-- MAYBE(pierre): Could create data for it which increases cost by one for each
-- fmap?
type CrabEngineering = (FuelCost, StepCost)

data Part
  = Part1
  | Part2
  deriving (Eq)

ex1 :: CrabMap -> [CrabPosition] -> Part -> FuelCost
ex1 crabs positions part =
  let costs = map (calcFuelCost crabs part) positions
      minimalCost =
        foldl1
          (\lowestCost cost ->
             if cost < lowestCost
               then cost
               else lowestCost)
          costs
   in minimalCost

calcFuelCost :: CrabMap -> Part -> CrabPosition -> FuelCost
calcFuelCost crabs part goalPosition =
  M.foldlWithKey
    (\cost crabPosition crabCount ->
       cost + (getCost crabCount crabPosition goalPosition part))
    0
    crabs
  -- where

getCost :: Int -> CrabPosition -> CrabPosition -> Part -> Int
getCost crabCount crabPosition goalPosition part =
  if part == Part1
    then crabCount * (abs (crabPosition - goalPosition))
    else let positions =
               case compare crabPosition goalPosition of
                 EQ -> []
                -- Walk till -1 since we don't need fuel once we reached the
                -- goal. Example: 0 .. 2 --> Need fuel from 0 to 1, then from 1
                -- to 2. But not from 2 to 2 (which would be applied using
                -- foldl).
                 LT -> [crabPosition .. goalPosition - 1]
                 GT -> [goalPosition .. crabPosition - 1]
          in crabCount *
             (fst $
              foldl
                (\(fuelCost, stepCost) _ -> (fuelCost + stepCost, stepCost + 1))
                (0, 1)
                positions)

toInt :: String -> Int
toInt = read
