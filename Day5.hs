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
-- for range
import Data.Ix

-- for range
import Data.List
import Data.List.Index

-- for spliOn
import Data.List.Split

type Row = [Int]

type Grid = [Row]

main
  -- input <- readFile "testInput"
 = do
  input <- readFile "testInput"
  let rows = filter validRow . map (createRanges . parseRow) $ lines input
      points = filter validPoints . map (createPoints . parseRow) $ lines input
      grid = makeGrid
      -- resultingGrid = foldl (\grid row -> markGrid grid row) grid rows
      verticalRow = head $ tail $ tail rows
      horizontalTuple = head $ points
      verticalTuple = head $ tail $ tail points
  print points
  -- print $ markGrid grid horizontalPoint
  print verticalTuple
  print $ getPoints verticalTuple
  print $ markGrid grid $ getPoints verticalTuple
  print horizontalTuple
  print $ getPoints horizontalTuple
  print $ markGrid grid $ getPoints horizontalTuple
  -- print $ foldl (\acc row -> acc + countHits row) 0 resultingGrid
  return ()

type Point = (Int, Int)

type Walk = [Point]

-- Checks whether the points can form a diagonal, horizontal, or vertical
-- connection.
-- NOTE: can use uncurry to transform a function which takes two arguments to a
-- function which takes a tuple consisting of these two arguments!
validPoints :: (Point, Point) -> Bool
validPoints =
  or . sequenceA (map uncurry [isDiagonal, isHorizontal, isVertical])

-- testq = [isDiagonal, isHorizontal, isVertical]
--
-- testStuff = or . sequenceA (map uncurry [isDiagonal, isHorizontal, isVertical])
isDiagonal (x1, y1) (x2, y2) = x2 - x1 == y2 - x1

isHorizontal (x1, y1) (x2, y2) = y1 == y2

isVertical (x1, y1) (x2, y2) = x1 == x2

testDiagonal point1 point2 = getDiagonalPoints point1 point2 []

getDiagonalPoints :: Point -> Point -> Walk -> Walk
getDiagonalPoints p1@(x1, y1) p2@(x2, y2) walk
  -- is not diagonal
  | x2 - x1 /= y2 - y1 = []
  -- reached the goal
  | p1 == p2 = p1 : walk
  -- MAYBE(pierre): how would we implement '>' for points? Would need to make
  -- point data I think? Could create Point and implement fmap and > for it.
  -- walk towards goal
  | otherwise =
    if x1 > x2
      then getDiagonalPoints p1 (x2 + 1, y2 + 1) (p2 : walk)
      else getDiagonalPoints (x1 + 1, y1 + 1) p2 (p1 : walk)

-- Parses input, returns the required values in format [x1,y1,x2,y2].
parseRow :: String -> [Int]
parseRow = map toInt . filter (/= "") . splitOneOf ",-> "

createPoints :: [Int] -> (Point, Point)
createPoints (x1:y1:x2:y2:xs) = ((x1, y1), (x2, y2))

getPoints :: (Point, Point) -> [Point]
getPoints (p1, p2)
  | isHorizontal p1 p2 = getHorizontalPoints p1 p2 []
  | isVertical p1 p2 = getVerticalPoints p1 p2 []
  | isDiagonal p1 p2 = getDiagonalPoints p1 p2 []

getVerticalPoints :: Point -> Point -> Walk -> Walk
getVerticalPoints (x1, y1) (x2, y2) walk =
  getHorizontalPoints (y1, x1) (y2, x2) walk

getHorizontalPoints :: Point -> Point -> Walk -> Walk
getHorizontalPoints p1@(x1, y1) p2@(x2, y2) walk
    -- is not horizontal
  | y1 /= y2 = []
    -- reached the goal
  | x1 == x2 = p1 : walk
  | otherwise =
    if x1 > x2
      then getHorizontalPoints p1 (x2 + 1, y2) (p2 : walk)
      else getHorizontalPoints (x1 + 1, y1) p2 (p1 : walk)

-- getRanges :: (Point, Point) -> ([Int], [Int])
-- getRanges ((x1, y1), (x2, y2)) = (makeRange x1 x2, makeRange y1 y2)
-- Takes a parsed row and returns a tuple including the calculated ranges for
-- the given x1,x2 and y1,y2 values.
createRanges :: [Int] -> ([Int], [Int])
createRanges (x1:y1:x2:y2:xs) = (makeRange x1 x2, makeRange y1 y2)
  where
    makeRange start end =
      if start > end
        then range (end, start)
        else range (start, end)
createRanges _ = ([], [])

-- Checks if a given row contains useful information for us. We only check
-- vertical or horizontal rows.
validRow :: ([Int], [Int]) -> Bool
validRow row = isHorizontalRow row || isVerticalRow row

isHorizontalRow (xRange, yRange) = length yRange == 1

isVerticalRow (xRange, yRange) = length xRange == 1

-- create 1000x1000 grid (based on checking input values)
makeGrid :: Grid
makeGrid =
  let row = take 10 [1 ..]
   in map (\_ -> take 10 [0,0 ..]) row

-- markGrid :: Grid -> ([Int], [Int]) -> Grid
markGrid :: Grid -> [Point] -> Grid
markGrid grid [] = grid
markGrid grid ((x, y):points) =
  let (leftRows, row:rightRows) = splitAt y grid
      (leftVals, point:rightVals) = splitAt x row
      newRow = leftVals ++ point + 1 : rightVals
      newGrid = leftRows ++ newRow : rightRows
   in markGrid newGrid points

markRow :: Row -> [Int] -> Row
markRow row positions =
  let startPos = head positions
      endPos = last positions
      (left, rest) = splitAt startPos row
      (vals, right) = splitAt (1 + endPos - startPos) rest
   in left ++ map (+ 1) vals ++ right

countHits :: Row -> Int
countHits =
  sum .
  map
    (\x ->
       if x > 1
         then 1
         else 0)

toInt :: String -> Int
toInt = read
-- markGrid grid row@(xRange, yRange)
--   -- note: order matters since we implement vertical rows by multiple horizonal
--   -- row markings. with stuff like ([2],[3]) ([2],[4]) etc.. for a verticalRow
--   -- marking for something like ([2],[3,4])
--   | isHorizontalRow row =
--     let splitPos = head yRange
--         (left, currentRow:right) = splitAt splitPos grid
--      in left ++ [markRow currentRow xRange] ++ right
--   | isVerticalRow row =
--     foldl (\grid val -> markGrid grid (xRange, [val])) grid yRange -- TODO: continue here! 45 degree only.
--   -- | isDiagonalRow row =
