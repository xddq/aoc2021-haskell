{-|
Example data:
    Before any steps:
    11111
    19991
    19191
    19991
    11111

    After step 1:
    34543
    40004
    50005
    40004
    34543

    After step 2:
    45654
    51115
    61116
    51115
    45654
    - if 9 is reached in any step, increase value for each adjacent (horizontal,
vertical, diagonal)
    - values can be increased up to 9 times per step
    - point can only flash ones per step.
    - NOTE: reuse stuff from Day9 for this.
-}
import Data.Map (Map)
import qualified Data.Map as M

-- imports only digitToInt from Data.Char
-- https://wiki.haskell.org/Import
import Data.Char (digitToInt)
import Data.List (foldl', nub, transpose)

main
  -- input <- lines <$> readFile "testInput"
 = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  -- print $ ex2 input
  return ()

iterations = 2

-- ex1 :: [[Char]] -> Int
ex1 input =
  let grid = getGrid input
      flashingPoints = getFlashingPoints grid
   -- in increaseValues grid $ getAllPulsatingPoints grid flashingPoints
   in foldl
        (\grid _ ->
           increaseValues grid $
           getAllPulsatingPoints grid $ getFlashingPoints grid)
        grid
        [1 .. iterations]
  where
    increaseValues :: Grid -> [Point] -> Grid
    increaseValues =
      foldl
        (\grid point ->
           insertValue grid point (increaseValue $ getValue grid point))

-- recursively gets flashing points until the amount of new flashing points is
-- zero.
getAllPulsatingPoints grid pulsatingPoints =
  let neighbours =
        nub $
        foldl'
          (\points point -> points ++ getNeighbouringPoints grid point)
          []
          pulsatingPoints
      filteredNeighbours =
        filter (\point -> (getValue grid point) /= edgeValue) neighbours
      allowedNeighbours =
        filter
          (\neighbour -> not $ neighbour `elem` pulsatingPoints)
          filteredNeighbours
   in if null allowedNeighbours
        then pulsatingPoints
        else getAllPulsatingPoints grid (allowedNeighbours ++ pulsatingPoints)

increaseValue value
  | value == 9 = 0
  | value == edgeValue = edgeValue
  | otherwise = value + 1

-- mostly reused stuff from Day9. Adapted here and there.
type Grid = Map Int (Map Int Int)

type Point = (Int, Int)

edgeValue = 100

pulsateValue = 100

getNeighbouringPoints :: Grid -> Point -> [Point]
getNeighbouringPoints grid (x, y) =
  let colCount = length $ M.keys grid
      rowCount =
        case M.lookup 0 grid of
          Just col -> length $ M.keys col
      points =
        [ (x + 1, y)
        , (x - 1, y)
        , (x, y + 1)
        , (x, y - 1)
        , (x + 1, y + 1)
        , (x - 1, y - 1)
        , (x + 1, y - 1)
        , (x - 1, y + 1)
        ]
   in filter
        (\(x, y) -> x > -1 && x < colCount && y > -1 && y < rowCount)
        points

getFlashingPoints :: Grid -> [Point]
getFlashingPoints grid =
  let colCount = length $ M.keys grid
      colRange = [0 .. colCount - 1]
      rowCount =
        case M.lookup 0 grid of
          Just col -> length $ M.keys col
          Nothing -> error "could not get colCount at getLowPoints."
      rowRange = [0 .. rowCount - 1]
      points =
        filter (\tuple -> fst tuple /= -1) $
        concatMap
          (\x ->
             map
               (\y ->
                  if isFlashingPoint grid (x, y)
                    then (x, y)
                    else (-1, -1))
               rowRange)
          colRange
   in points

isFlashingPoint :: Grid -> Point -> Bool
isFlashingPoint grid (x, y)
    -- checks if we have lower value than all 4 cells around us.
 =
  case M.lookup x grid of
    Just col ->
      case M.lookup y col of
        Just val -> val == 9

insertValue :: Grid -> Point -> Int -> Grid
insertValue grid (x, y) val =
  let innerMap = M.lookup x grid
   in case innerMap of
        Just innerMap ->
          let newInnerMap = M.insert y val innerMap
           in M.insert x newInnerMap grid

-- gets value for an entry in our grid/map of maps. If we are looking outside of
-- our grid, return the edge value.
getValue :: Grid -> Point -> Int
getValue grid (x, y) =
  case M.lookup x grid of
    Just col ->
      case M.lookup y col of
        Just val -> val
        Nothing -> edgeValue
    Nothing -> edgeValue

-- Builds a grid consisting of a Map of Maps. Using this we can lookup the
-- values with a given x and y value.
getGrid :: [[Char]] -> Grid
getGrid input =
  let rows = map (\row -> map digitToInt row) input
      -- transforms input to list of rows with each row having their list of
      -- (key,value).
      colCount = length $ transpose rows
      rowCount = length rows
      resultRows =
        map (\key -> map (\val -> (key, (val !! key))) rows) [0 .. colCount - 1]
      resultCols = transpose resultRows
      grid =
        foldl
          (\newMap key ->
             M.insert
               key
               (makeMap $ getKeysAndValues (resultCols !! key))
               newMap)
          M.empty
          [0 .. (length resultCols) - 1]
   in grid

getKeysAndValues :: [Point] -> ([Int], [Int])
getKeysAndValues =
  foldr (\(key, value) (keys, values) -> (key : keys, value : values)) ([], [])

makeMap (keys, values) =
  foldl (\newMap key -> M.insert key (values !! key) newMap) M.empty keys
