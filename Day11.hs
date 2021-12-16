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
import Data.List

main
  -- input <- lines <$> readFile "testInput"
 = do
  input <- lines <$> readFile "inputDay11"
  print $ ex1 input
  print $ ex2 input
  return ()

ex1 :: [[Char]] -> Int
ex1 input =
  let grid = getGrid input
   in pulsate grid 100 0

ex2 :: [[Char]] -> Int
ex2 input =
  let grid = getGrid input
   in case pulsate2 grid 0 0 500 of
        Nothing -> error "did not find a step where all pulsated!"
        Just step -> step

allZero :: Grid -> Bool
allZero grid =
  let rowCount = length $ M.keys grid
      colCount =
        case M.lookup 0 grid of
          Just row -> length $ M.keys row
          Nothing -> error "could not get colCount at allZero."
   in rowCount * colCount == (length $ getValuesFromGrid grid (== 0))

-- similar to pulsate. But stops after the result of pulsating is a grid full of
-- zeros (all pulsated) and returns the step count.
pulsate2 :: Grid -> Int -> Int -> Int -> Maybe Int
pulsate2 grid steps count limit
  | allZero grid = Just steps
  | steps == limit = Nothing
  | otherwise =
    let pulsatedGrid = doPulsate $ operateOnGrid grid (+ 1)
        countPulsated = length $ getValuesFromGrid pulsatedGrid (didPulsate)
     in pulsate2
          (resetPulsated pulsatedGrid)
          (steps + 1)
          (count + countPulsated)
          limit
  where
    resetPulsated grid =
      operateOnGrid
        grid
        (\x ->
           if didPulsate x
             then 0
             else x)
    didPulsate val = val >= pulsatedMarker && val < 0

-- pulsates for a given amount of steps. returns the total count of pulsations.
pulsate :: Grid -> Int -> Int -> Int
pulsate grid steps count
  | steps == 0 = count
  | otherwise =
    let pulsatedGrid = doPulsate $ operateOnGrid grid (+ 1)
        countPulsated = length $ getValuesFromGrid pulsatedGrid (didPulsate)
     in pulsate (resetPulsated pulsatedGrid) (steps - 1) (count + countPulsated)
  where
    resetPulsated grid =
      operateOnGrid
        grid
        (\x ->
           if didPulsate x
             then 0
             else x)
    didPulsate val = val >= pulsatedMarker && val < 0

-- applies given function to every value in grid.
operateOnGrid :: Grid -> (Int -> Int) -> Grid
operateOnGrid grid f =
  let rows = M.keys grid
   in M.foldlWithKey
        (\rowMap row colMap ->
           M.insert
             row
             (M.foldlWithKey
                (\colMap col val -> M.insert col (f val) colMap)
                M.empty
                colMap)
             rowMap)
        M.empty
        grid

-- pulsates for all points inside the grid
doPulsate :: Grid -> Grid
doPulsate grid =
  let next = getNextPulsatedPoint grid
   in case next of
        Just point ->
          let newGrid =
                operateOnPoints
                  (insertValue grid point pulsatedMarker)
                  (getNeighbouringPoints grid point)
                  (+ 1)
           in doPulsate newGrid
        Nothing -> grid

-- MAYBE(pierre): Adapt to only find the next one instead of all and get the
-- head. Could do after solution works.
getNextPulsatedPoint :: Grid -> Maybe Point
getNextPulsatedPoint grid = do
  let next = getValuesFromGrid grid (> 9)
  case length next of
    0 -> Nothing
    _ -> Just (head next)

-- gets all values from the grid for which the predicate is True
getValuesFromGrid :: Grid -> (Int -> Bool) -> [Point]
getValuesFromGrid grid f =
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
                  if f $ getValue grid (x, y)
                    then (x, y)
                    else (-1, -1))
               rowRange)
          colRange
   in points

-- applies function to all given points in a grid
operateOnPoints :: Grid -> [Point] -> (Int -> Int) -> Grid
operateOnPoints grid points f =
  foldl
    (\grid point -> insertValue grid point (f $ getValue grid point))
    grid
    points

-- mostly reused stuff from Day9. Adapted here and there.
type Grid = Map Int (Map Int Int)

type Point = (Int, Int)

pulsatedMarker = -127

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

insertValue :: Grid -> Point -> Int -> Grid
insertValue grid (x, y) val =
  let innerMap = M.lookup x grid
   in case innerMap of
        Just innerMap ->
          let newInnerMap = M.insert y val innerMap
           in M.insert x newInnerMap grid

-- gets value for an entry in our grid/map of maps. If we are looking outside of
-- our grid, return the edge value.
-- MAYBE(pierre): adapt to return Maybe instead of fixed int val in nothing
-- case.
getValue :: Grid -> Point -> Int
getValue grid (x, y) =
  case M.lookup x grid of
    Just col ->
      case M.lookup y col of
        Just val -> val
        Nothing ->
          error "this should not happen. Got value that was not in grid."
    Nothing -> error "this should not happen. Got value that was not in grid."

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
