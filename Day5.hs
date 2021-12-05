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

main = do
  input <- readFile "testInput"
  let rows = filter validRow . map (createRanges . parseRow) $ lines input
      grid = makeGrid
  -- y == 1 --> horizontalRow
  -- x == 1 --> verticalRow
  print $ head rows
  print $ markGrid grid (head rows)
  -- print $ foldl (\grid row -> markGrid grid row) grid rows
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

-- checks if a given row contains useful information for us. We only check
-- vertical or horizontal rows.
validRow :: ([Int], [Int]) -> Bool
validRow row = isHorizontalRow row || isVerticalRow row

isHorizontalRow (xRange, yRange) = length yRange == 1

isVerticalRow (xRange, yRange) = length xRange == 1

-- creates 10x10 grid for testing.
-- TODO(pierre): create 1000x1000 grid.
makeGrid :: Grid
makeGrid =
  let row = take 10 [1 ..]
   in map (\_ -> take 10 [0,0 ..]) row

markGrid :: Grid -> ([Int], [Int]) -> Grid
markGrid grid row@(xRange, yRange)
  | isHorizontalRow row =
    let splitPos = head xRange
        (left, currentRow:right) = splitAt splitPos grid
     in left ++ [markRow currentRow yRange] ++ right
  | isVerticalRow row = grid
    -- let splitPos = head yRange
    --     (left, currentRow:right) = splitAt splitPos grid
    --  in left ++ [markRow currentRow yRange] ++ right

-- markRow grid (head yRange) xRange
markRow :: Row -> [Int] -> Row
markRow row positions =
  let startPos = head positions
      endPos = last positions
      (left, rest) = splitAt startPos row
      (vals, right) = splitAt (endPos - startPos) rest
   in left ++ map (+ 1) vals ++ right

toInt :: String -> Int
toInt = read

type Row = [Int]

type Grid = [Row]

-- removes items from left and right of given list based on given predicate.
trimWith :: (a -> Bool) -> [a] -> [a]
trimWith pred = dropWhileEnd pred . dropWhile pred
