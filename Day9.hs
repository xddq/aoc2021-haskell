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
-- imports only digitToInt from Data.Char
-- https://wiki.haskell.org/Import
import Data.Char (digitToInt)
import Data.List (transpose)

type Row = [Maybe Int]

type Col = Row

-- prefix with P since Left and Right are already taken for Either!
data Position a
  = Direction PDirection a
  | PEdge
  | PBasin
  -- | PSkip -- added for ex2. Will be assigned to old lowest points.
  deriving (Show, Read, Eq)

data PDirection
  = PLeft
  | PAbove
  | PRight
  | PBelow
  | PMiddle
  deriving (Show, Read, Eq)

instance (Ord a, Eq a) => Ord (Position a) where
  compare (Direction _ x) (Direction _ y) = compare x y
  compare (PEdge) (Direction _ y) = LT
  compare (Direction _ y) (PEdge) = LT
  compare PEdge PEdge = EQ
  -- this case was added for Part2. Required since we replace old lowest points
  -- with PBasin.
  compare PBasin _ = GT
  compare _ PBasin = LT

-- MAYBE(pierre): Is it possibile to define that first positon must be
-- PLeft etc..?
type Point
   = (Position Int, Position Int, Position Int, Position Int, Position Int)

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
main = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  print $ ex2 input
  -- print $ getPoints input
  -- print $ filter isLowPoint (getPoints input)
  -- print $ makeNewPoints (getPoints input)
  return () -- ex1 :: [Int] -> Int

-- ex2 :: [[Char]] -> Int
ex2 input =
  let points = getPoints input
      lowPoints = filter isLowPoint points
   in makeBasin points lowPoints

-- makeBasin :: [Point] -> Point -> [Int]
-- makeBasin [] point = []
makeBasin points lowPoints =
  let newPointss = map (`addBasin` points) lowPoints
      newLowPointss = map (filter isLowPoint) newPointss
    -- maybe check if point is 9
      -- newLowPoints = filter isLowPoint newPoints
   in newPointss

addBasin :: Point -> [Point] -> [Point]
addBasin basinPoint =
  map
    (\x ->
       if x == basinPoint
         then markAsBasin x
         else x)

markAsBasin =
  (\(left, above, middle, right, below) -> (left, above, PBasin, right, below))

makeNewPoints :: [Point] -> [Point]
makeNewPoints [] = []
makeNewPoints (point:points)
  | point `elem` (filter isLowPoint (point : points)) =
    (makeMiddleEdge point) : makeNewPoints points
  | otherwise = point : makeNewPoints points

makeMiddleEdge =
  (\(left, above, middle, right, below) -> (left, above, PEdge, right, below))

-- Plan for part two: take list of points
-- get list of lowest points
-- calc risk --> (sum map (+1) listLowestPoints)
-- replace the lowest points in the list of points with their middle being an edge and
-- then find the lowest points for the new list of points! (add them
-- recursively)
-- if we have an empty list, return 0
ex1 :: [[Char]] -> Int
ex1 input =
  let rows =
        map
          (parseRow (\pos -> Direction PLeft pos) (\pos -> Direction PRight pos))
          input
      colInput = transpose input
      cols =
        map
          (parseRow
             (\pos -> Direction PAbove pos)
             (\pos -> Direction PBelow pos))
          colInput
      -- MAYBE(pierre): Figure out why this did not work.
      -- combinedResult =
      --   [ (left, above, middle, right, below)
      --   | ((left, middle, right):row) <- rows
      --   , ((above, _, below):col) <- transpose cols
      --   ]
      combinedResult =
        zipWith
          (\row col ->
             zipWith
               (\(left, middle, right) (above, _, below) ->
                  (left, above, middle, right, below))
               row
               col)
          rows
          (transpose cols)
   in sum $
      map (+ 1) $
      concatMap (\row -> map getMiddle row) $
      map (filter isLowPoint) combinedResult

getPoints :: [[Char]] -> [Point]
getPoints input =
  let rows =
        map
          (parseRow (\pos -> Direction PLeft pos) (\pos -> Direction PRight pos))
          input
      colInput = transpose input
      cols =
        map
          (parseRow
             (\pos -> Direction PAbove pos)
             (\pos -> Direction PBelow pos))
          colInput
      combinedResult =
        concat $
        zipWith
          (\row col ->
             zipWith
               (\(left, middle, right) (above, _, below) ->
                  (left, above, middle, right, below))
               row
               col)
          rows
          (transpose cols)
   in combinedResult

-- getMiddle2 :: Point -> Maybe Int
-- getMiddle2 (left, above, Direction _ val, right, below) = Just val
-- getMiddle2 (left, above, _, right, below) = Nothing
getMiddle :: Point -> Int
getMiddle (left, above, Direction _ val, right, below) = val

isLowPoint :: Point -> Bool
isLowPoint (left, above, middle, right, below) =
  (all (== LT) $ map (compare middle) [left, above, right, below]) &&
  middle /= PEdge

parseRow ::
     (Int -> Position Int)
  -> (Int -> Position Int)
  -> [Char]
  -> [(Position Int, Position Int, Position Int)]
parseRow left right input =
  let row = map (\digit -> digitToInt digit) input
   in zip3
        (PEdge : map left row)
        (map (Direction PMiddle) row)
        ((drop 1 $ map right row) ++ [PEdge]) -- original data structure, could not implement Ord!
-- data Position a
--   = PLeft a
--   | PAbove a
--   | PRight a
--   | PBelow a
--   | PMiddle a
--   | PEdge
--   deriving (Show, Read, Eq)
