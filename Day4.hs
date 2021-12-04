-- example data
-- 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
--
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19
--
--  3 15  0  2 22
--  9 18 13 17  5
-- 19  8  7 25 23
-- 20 11 10 24  4
-- 14 21 16 12  6
--
-- 14 21 17 24  4
-- 10 16 15  9 19
-- 18  8 23 26 20
-- 22 11 13  6  5
--  2  0 12  3  7
--  for groupBy
import Data.Char
import Data.List

main = do
  input <- readFile "inputDay4"
  -- gets all rows, filters empty lines
  let rows = filter (not . (== "")) $ lines input
  -- gets the numbers we will draw
  let numbersRow = head rows
  -- converts them to list of int values
  let numbers =
        map toInt . map (trimWith isPunctuation) $
        splitOn (not . isPunctuation) numbersRow
  let boards = createBoards $ drop 1 $ rows
  -- print boards
  print $ ex1 boards numbers
  return ()

toInt :: String -> Int
toInt val = read val

type Mark = Bool

type Cell = (Int, Mark)

type Row = [Cell]

type Board = [Row]

-- TODO: should be called splitOnNot or something. change logic or naming later.
splitOn pred = groupBy (\_ next -> pred next)

trim = dropWhileEnd isSpace . dropWhile isSpace

trimWith pred = dropWhileEnd pred . dropWhile pred

markBoard :: Int -> Board -> Board
markBoard number board =
  map
    (\row ->
       map
         (\(val, mark) ->
            if val == number
              then (val, True)
              else (val, mark))
         row)
    board

makeCell :: Int -> Cell
makeCell val = (val, False)

isMarked :: Cell -> Bool
isMarked (_, marked) = marked

makeRow :: [Char] -> Row
makeRow =
  map makeCell .
  map toInt . map trim . filter (not . (all isSpace)) . splitOn (not . isSpace)
  where
    toInt x = read x :: Int

unmarkedNumbers :: Board -> Board
unmarkedNumbers board = map (\row -> filter (not . isMarked) row) board

sumOfBoard :: Board -> Int
sumOfBoard board = sum $ map (\row -> sum $ map (\(val, _) -> val) row) board

-- DEBUG:
-- sumOfBoard works.
-- unmarkedNumbers works.
-- numbers works.
-- finds winning board and calculates the result
ex1 :: [Board] -> [Int] -> Int
ex1 boards (x:numbers) =
  let markedBoards = map (markBoard x) boards
      winningBoards = filter isWinningBoard markedBoards
      winningBoard = head winningBoards
   in if null winningBoards
        then ex1 markedBoards numbers
        else x * (sumOfBoard $ unmarkedNumbers $ head winningBoards)

-- testboard
testBoard :: Board
testBoard =
  [ [(65, True), (24, True), (23, True), (1, True), (19, False)]
  , [(54, True), (35, True), (76, True), (71, True), (49, False)]
  , [(53, True), (34, True), (75, True), (70, True), (48, False)]
  , [(10, True), (75, True), (99, True), (91, True), (97, False)]
  , [(21, True), (78, True), (17, True), (18, True), (81, False)]
  ]

testRows =
  [ "65 24 23  1 19"
  , "54 35 76 71 49"
  , "53 34 75 70 48"
  , "10 75 99 91 97"
  , "21 78 17 18 81"
  ]

createBoards :: [String] -> [Board]
createBoards (r1:r2:r3:r4:r5:rows) =
  let board = map makeRow (r1 : r2 : r3 : r4 : [])
   in [board] ++ createBoards rows
createBoards _ = []

isWinningBoard :: Board -> Bool
isWinningBoard board =
  let rows = board
      cols = transpose board
   in any isWinningRow $ rows ++ cols

isWinningRow :: Row -> Bool
isWinningRow = all isMarked
