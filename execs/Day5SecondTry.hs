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
-- for splitOn
import Data.List.Split

-- MAYBE(pierre): Use Map as type instead of 1000x1000 grid.. :D
-- import Data.Map (Map)
type Row = [Int]

type Grid = [Row]

type Point = (Int, Int)

type Walk = [Point]

main = do
  input <- readFile "inputDay5"
  let points = filter (not . null) . map (parsePoints . parseRow) $ lines input
      grid = makeGrid
  print $ countHits $ markGrid grid $ concatMap (getPoints Part1) points
  -- mapM_ print $ concatMap (getPoints Part2) points
  -- mapM_ print $ markGrid grid $ concatMap (getPoints Part2) points
  print $ countHits $ markGrid grid $ concatMap (getPoints Part2) points
  return ()

data Mode
  = Part1
  | Part2
  deriving (Eq)

-- Parses input, returns the required values in format [x1,y1,x2,y2].
parseRow :: String -> [Int]
parseRow = map toInt . filter (/= "") . splitOneOf ",-> "

-- Parses input, transforms list in format [x1,y1,x2,y2] to ((x1,y1),(x2,y2))
parsePoints :: [Int] -> (Point, Point)
parsePoints (x1:y1:x2:y2:xs) = ((x1, y1), (x2, y2))

-- Returns list of all points which connect the two given points (inclusive).
getPoints :: Mode -> (Point, Point) -> [Point]
getPoints mode (p1@(x1, y1), p2@(x2, y2))
  -- is horizontal
  | y1 == y2 =
    if x1 < x2
      then zip [x1 .. x2] (repeat y1)
      else zip (reverse [x2 .. x1]) (repeat y1)
  -- is vertical
  | x1 == x2 =
    if y1 < y2
      then zip (repeat x1) [y1 .. y2]
      else zip (repeat x2) (reverse [y2 .. y1])
  | otherwise
    -- check if ex2 and is diagonal
   =
    if mode == Part2 && abs (x2 - x1) == abs (y2 - y1)
      then getDiagonalPoints p1 p2
      else []

getDiagonalPoints :: Point -> Point -> [Point]
getDiagonalPoints p1@(x1, y1) p2@(x2, y2)
  | x1 > x2 && y1 > y2 = zip (reverse [x2 .. x1]) (reverse [y2 .. y1])
  | x1 > x2 && y1 < y2 = zip (reverse [x2 .. x1]) [y1 .. y2]
  | x1 < x2 && y1 > y2 = zip [x1 .. x2] (reverse [y2 .. y1])
  | x1 < x2 && y1 < y2 = zip [x1 .. x2] [y1 .. y2]
  | otherwise = error "undefined case in getDiagonalPoints"

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
