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
        splitOn isPunctuation numbersRow
  let boards = createBoards $ drop 1 $ rows
  -- print numbers
  print $ ex1 boards numbers
  return ()

toInt :: String -> Int
toInt val = read val

type Mark = Bool

type Cell = (Int, Mark)

type Row = [Cell]

type Board = [Row]

splitOn pred = groupBy (\_ next -> (not . pred) next)

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
  map toInt . map trim . filter (not . (all isSpace)) . splitOn isSpace
  where
    toInt x = read x :: Int

unmarkedNumbers :: Board -> Board
unmarkedNumbers board = map (\row -> filter (not . isMarked) row) board

sumOfBoard :: Board -> Int
sumOfBoard board = sum $ map (\row -> sum $ map (\(val, _) -> val) row) board

-- finds winning board and calculates the result
ex1 :: [Board] -> [Int] -> Int
ex1 boards (x:numbers) =
  let markedBoards = map (markBoard x) boards
      winningBoards = filter isWinningBoard markedBoards
      winningBoard = head winningBoards
   in if null winningBoards
        then ex1 markedBoards numbers
        else x * (sumOfBoard $ unmarkedNumbers $ head winningBoards)

createBoards :: [String] -> [Board]
createBoards (r1:r2:r3:r4:r5:rows) =
  let board = map makeRow (r1 : r2 : r3 : r4 : r5 : [])
   in [board] ++ createBoards rows
createBoards _ = []

isWinningBoard :: Board -> Bool
isWinningBoard board =
  let rows = board
      cols = transpose board
   in any isWinningRow $ rows ++ cols

isWinningRow :: Row -> Bool
isWinningRow = all isMarked
-- ex1 :: [Board] -> [Int] -> Int
-- ex1 boards (x:numbers) =
--   let markedBoards = map (markBoard x) boards
--       winningBoards = filter isWinningBoard markedBoards
--       winningBoard = head winningBoards
--    in if null winningBoards
--         then ex1 markedBoards numbers
--         else x * (sumOfBoard $ unmarkedNumbers $ head winningBoards)
