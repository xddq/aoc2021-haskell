{-|
Example data:
    2199943210
    3987894921
    9856789892
    8767896789
    9899965678
        - Each number corresponds to the height of a particular location, where 9
is the highest and 0 is the lowest a location can be.
        - Find the low points (locations where any other adjacent (horizontal or
        vertical) point has a value that is lower than the current(EQ does not
count as lower).
        - In the above example, there are four low points
        - risk level of a low point is 1 plus its height
        - get the sum of risk levels of all low points.
-}
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromMaybe)

-- imports only digitToInt from Data.Char
-- https://wiki.haskell.org/Import
import Data.Char (digitToInt)
import Data.List (foldl', nub, sortBy, transpose)

main
  -- input <- lines <$> readFile "testInput"
 = do
  input <- lines <$> readFile "inputDay9"
  print $ ex1 input
  print $ ex2 input
  return ()

type Grid = Map Int (Map Int Int)

type Point = (Int, Int)

ex1 input =
  let grid = getGrid input
      lowPoints = getLowPoints grid
   in foldl (\risk point -> risk + getRisk grid point) 0 $ getLowPoints grid

getRisk :: Grid -> Point -> Int
getRisk grid (x, y) =
  case M.lookup x grid of
    Just row ->
      case M.lookup y row of
        Just val -> val + 1

ex2 :: [[Char]] -> Int
ex2 input =
  let grid = getGrid input
      lowPoints = getLowPoints grid
      basins = map (\point -> makeBasin grid [point]) lowPoints
   in product $ take 3 $ sortBy (flip compare) $ map length basins

-- Value of points at the edges (outside of our grid).
edgeValue = 100

-- Points with this value do not form a basin.
noBasinValue = 9

makeBasin :: Grid -> [Point] -> [Point]
makeBasin grid visitedPoints =
  let neighbours =
        nub $
        foldl'
          (\points point -> points ++ getNeighbouringPoints grid point)
          []
          visitedPoints
      filteredNeighbours =
        filter
          (\point ->
             getValue grid point /= noBasinValue &&
             getValue grid point /= edgeValue)
          neighbours
      -- only use points that are not already inside our basin
      allowedNeighbours = filter (`notElem` visitedPoints) filteredNeighbours
   in if null allowedNeighbours
        then visitedPoints
        else makeBasin grid (allowedNeighbours ++ visitedPoints)

getNeighbouringPoints :: Grid -> Point -> [Point]
getNeighbouringPoints grid (x, y) =
  let colCount = length $ M.keys grid
      rowCount =
        case M.lookup 0 grid of
          Just col -> length $ M.keys col
      points = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
   in filter
        (\(x, y) -> x > -1 && x < colCount && y > -1 && y < rowCount)
        points

getLowPoints :: Grid -> [Point]
getLowPoints grid =
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
                  if isLowPoint grid (x, y)
                    then (x, y)
                    else (-1, -1))
               rowRange)
          colRange
   in points

isLowPoint :: Grid -> Point -> Bool
isLowPoint grid (x, y)
    -- checks if we have lower value than all 4 cells around us.
 =
  case M.lookup x grid of
    Just col ->
      case M.lookup y col of
        Just val ->
          all
            (getValue grid (x, y) <)
            [ getValue grid (x - 1, y)
            , getValue grid (x + 1, y)
            , getValue grid (x, y + 1)
            , getValue grid (x, y - 1)
            ]

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
    Just col -> fromMaybe edgeValue (M.lookup y col)
    Nothing -> edgeValue

-- Builds a grid consisting of a Map of Maps. Using this we can lookup the
-- values with a given x and y value.
getGrid :: [[Char]] -> Grid
getGrid input =
  let rows = map (map digitToInt) input
      -- transforms input to list of rows with each row having their list of
      -- (key,value).
      colCount = length $ transpose rows
      rowCount = length rows
      resultRows =
        map (\key -> map (\val -> (key, val !! key)) rows) [0 .. colCount - 1]
      resultCols = transpose resultRows
      grid =
        foldl
          (\newMap key ->
             M.insert
               key
               (makeMap $ getKeysAndValues (resultCols !! key))
               newMap)
          M.empty
          [0 .. length resultCols - 1]
   in grid

getKeysAndValues :: [Point] -> ([Int], [Int])
getKeysAndValues =
  foldr (\(key, value) (keys, values) -> (key : keys, value : values)) ([], [])

makeMap (keys, values) =
  foldl (\newMap key -> M.insert key (values !! key) newMap) M.empty keys
