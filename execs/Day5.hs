-- example data
-- 0,9 -> 5,9
-- 8,0 -> 0,8
-- 9,4 -> 3,4
-- 2,2 -> 2,1
-- 7,0 -> 7,4
-- 6,4 -> 2,0
-- 0,9 -> 2,9
-- 3,4 -> 1,4
-- 0,0 -> 8,8
-- 5,5 -> 8,2
-- .......1..
-- ..1....1..
-- ..1....1..
-- .......1..
-- .112111211
-- ..........
-- ..........
-- ..........
-- ..........
-- 222111....
-- for spliOn
import Data.List.Split

type Row = [Int]

type Grid = [Row]

type Point = (Int, Int)

type Walk = [Point]

main = do
  input <- readFile "inputDay5"
  let points = filter (not . null) . map (parsePoints . parseRow) $ lines input
      grid = makeGrid
  print $ countHits $ markGrid grid $ concatMap getPoints points
  return ()

isDiagonal (x1, y1) (x2, y2) = abs (x2 - x1) == abs (y2 - y1)

isHorizontal (x1, y1) (x2, y2) = y1 == y2

isVertical (x1, y1) (x2, y2) = x1 == x2

-- Parses input, returns the required values in format [x1,y1,x2,y2].
parseRow :: String -> [Int]
parseRow = map toInt . filter (/= "") . splitOneOf ",-> "

-- Parses input, transforms list in format [x1,y1,x2,y2] to ((x1,y1),(x2,y2))
parsePoints :: [Int] -> (Point, Point)
parsePoints (x1:y1:x2:y2:xs) = ((x1, y1), (x2, y2))

-- Returns list of all points which connect the two given points (inclusive).
getPoints :: (Point, Point) -> [Point]
getPoints (p1, p2)
  | isHorizontal p1 p2 = getHorizontalPoints p1 p2 []
  | isVertical p1 p2 = getVerticalPoints p1 p2 []
  | isDiagonal p1 p2 = getDiagonalPoints p1 p2 []
  | otherwise = []

getHorizontalPoints :: Point -> Point -> Walk -> Walk
getHorizontalPoints p1@(x1, y1) p2@(x2, y2) walk
    -- is not horizontal
  | y1 /= y2 = []
    -- reached the goal
  | x1 == x2 = p1 : walk
  -- walk towards goal
  | otherwise =
    if x1 > x2
      then getHorizontalPoints p1 (x2 + 1, y2) (p2 : walk)
      else getHorizontalPoints (x1 + 1, y1) p2 (p1 : walk)

getVerticalPoints :: Point -> Point -> Walk -> Walk
getVerticalPoints p1@(x1, y1) p2@(x2, y2) walk
    -- is not vertical
  | x1 /= x2 = []
    -- reached the goal
  | y1 == y2 = p1 : walk
  -- walk towards goal
  | otherwise =
    if y1 > y2
      then getVerticalPoints p1 (x2, y2 + 1) (p2 : walk)
      else getVerticalPoints (x1, y1 + 1) p2 (p1 : walk)

getDiagonalPoints :: Point -> Point -> Walk -> Walk
getDiagonalPoints p1@(x1, y1) p2@(x2, y2) walk
  -- is not diagonal
  | not $ isDiagonal p1 p2 = []
  -- reached the goal
  | p1 == p2 = p1 : walk
  -- walk towards goal
  | otherwise =
    let (nextP1, nextP2, oldP) = walkDiagonal p1 p2
     in getDiagonalPoints nextP1 nextP2 (oldP : walk)

walkDiagonal :: Point -> Point -> (Point, Point, Point)
walkDiagonal p1@(x1, y1) p2@(x2, y2)
  | x1 > x2 && y1 > y2 = (p1, (x2 + 1, y2 + 1), p2)
  | x1 > x2 && y1 < y2 = (p1, (x2 + 1, y2 - 1), p2)
  | x1 < x2 && y1 > y2 = (p1, (x2 - 1, y2 + 1), p2)
  | x1 < x2 && y1 < y2 = (p1, (x2 - 1, y2 - 1), p2)
  | x1 == x2 || y1 == x2 = (p1, p2, p2)

-- create 1000x1000 grid (based on checking input values)
makeGrid :: Grid
makeGrid =
  let row = take 1000 [1 ..]
   in map (\_ -> take 1000 [0,0 ..]) row

-- iterates through given points and increases value of cell for each given
-- point.
markGrid :: Grid -> [Point] -> Grid
markGrid grid [] = grid
markGrid grid ((x, y):points) =
  let (leftRows, row:rightRows) = splitAt y grid
      (leftVals, point:rightVals) = splitAt x row
      newRow = leftVals ++ point + 1 : rightVals
      newGrid = leftRows ++ newRow : rightRows
   in markGrid newGrid points

countHits :: Grid -> Int
countHits =
  sum .
  concat .
  map
    (\row ->
       map
         (\x ->
            if x > 1
              then 1
              else 0)
         row)

toInt :: String -> Int
toInt = read
-- Checks whether the points can form a diagonal, horizontal, or vertical
-- connection.
-- NOTE: can use uncurry to transform a function which takes two arguments to a
-- function which takes a tuple consisting of these two arguments!
-- validPoints :: (Point, Point) -> Bool
-- validPoints =
--   or . sequenceA (map uncurry [isDiagonal, isHorizontal, isVertical])
