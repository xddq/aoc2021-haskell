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
  input <- readFile "inputDay5"
  let rows = filter validRow . map (createRanges . parseRow) $ lines input
      grid = makeGrid
      resultingGrid = foldl (\grid row -> markGrid grid row) grid rows
  --     verticalRow = head $ tail $ tail rows
  -- print verticalRow
  -- print $ markGrid grid verticalRow
  print $ foldl (\acc row -> acc + countHits row) 0 resultingGrid
  return ()

-- Parses input, returns the required values in format [x1,y1,x2,y2].
parseRow :: String -> [Int]
parseRow = map toInt . filter (/= "") . splitOneOf ",-> "

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
  let row = take 1000 [1 ..]
   in map (\_ -> take 1000 [0,0 ..]) row

-- markGrid :: Grid -> ([Int], [Int]) -> Grid
markGrid grid row@(xRange, yRange)
  -- note: order matters since we implement vertical rows by multiple horizonal
  -- row markings. with stuff like ([2],[3]) ([2],[4]) etc.. for a verticalRow
  -- marking for something like ([2],[3,4])
  | isHorizontalRow row =
    let splitPos = head yRange
        (left, currentRow:right) = splitAt splitPos grid
     in left ++ [markRow currentRow xRange] ++ right
  | isVerticalRow row =
    foldl (\grid val -> markGrid grid (xRange, [val])) grid yRange-- TODO: continue here! 45 degree only.
  -- | isDiagonalRow row =

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
