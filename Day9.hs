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

-- imports only digitToInt from Data.Char
-- https://wiki.haskell.org/Import
import Data.Char (digitToInt)
import Data.List (foldl', nub, sortBy, transpose)

{-|
Initial Plan:
    - Build an easy to use data structure. Read input and build a grid from it.
    - Use zipping to create the following lists:
    - Use all rows of that grid to create a list of tuples (Just left, point,
Just right).
    - Use all cols of that grid (with transpose grid) to create a list of
tuples(Just above, point, Just below).
    - Zip these to get a list of tuples with (Just left, Just above, point, Just
right, Just below)
    - fold the grid to get the result (remember adding +1 for each point)
-}
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

-- ex2 :: [[Char]] -> Int
ex2 input =
  let grid = getGrid input
      lowPoints = getLowPoints grid
      firstPoint = head lowPoints
      secondPoint = head $ drop 1 $ lowPoints
   -- in makeBasin grid [(0, 0), (9, 9)]
      -- product $
      -- take 3 $
   in sortBy (flip compare) $
      map (\point -> length $ makeBasin grid [point]) lowPoints

makeBasin :: Grid -> [Point] -> [Point]
makeBasin grid visitedPoints =
  let neighbours =
        nub $
        foldl'
          (\points point -> points ++ getNeighbouringPoints grid point)
          []
          visitedPoints
      newGrid = foldl (\acc point -> insertValue acc point 9) grid visitedPoints
      allowedNeighbours =
        filter (\neighbour -> not $ neighbour `elem` visitedPoints) neighbours
      lowNeighbours = filter (isLowPoint newGrid) allowedNeighbours
   in if null lowNeighbours
        then visitedPoints
        else makeBasin newGrid (lowNeighbours ++ visitedPoints)

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
  let rowCount = length $ M.keys grid
      rowRange = [0 .. rowCount - 1]
      colCount =
        case M.lookup 0 grid of
          Just col -> length $ M.keys col
          Nothing -> error "could not get colCount at getLowPoints."
      colRange = [0 .. colCount - 1]
      points =
        filter (\tuple -> fst tuple /= -1) $
        concatMap
          (\x ->
             map
               (\y ->
                  if isLowPoint grid (x, y)
                    then (x, y)
                    else (-1, -1))
               colRange)
          rowRange
   in points

isLowPoint :: Grid -> Point -> Bool
isLowPoint grid (x, y)
    -- checks if cells around have lower value.
 =
  case M.lookup x grid of
    Just col ->
      case M.lookup y col of
        Just val ->
          val < 9 &&
          (all
             (getValue grid (x, y) <)
             [ getValue grid (x - 1, y)
             , getValue grid (x + 1, y)
             , getValue grid (x, y + 1)
             , getValue grid (x, y - 1)
             ])

insertValue :: Grid -> Point -> Int -> Grid
insertValue grid (x, y) val =
  let innerMap = M.lookup x grid
   in case innerMap of
        Just innerMap ->
          let newInnerMap = M.insert y val innerMap
           in M.insert x newInnerMap grid

getValue :: Grid -> Point -> Int
getValue grid (x, y)
  -- default to 9 if we are outside of grid to make sure comparing against edges
  -- will result in a low point (if the low point is < 9)
 =
  case M.lookup x grid of
    Just col ->
      case M.lookup y col of
        Just val -> val
        Nothing -> 9
    Nothing -> 9

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
