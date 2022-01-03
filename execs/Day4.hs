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
  let rows = filter (/= "") $ lines input
  -- gets the numbers we will draw
  let numbersRow = head rows
  -- converts them to list of int values
  let numbers =
        map (toInt . trimWith isPunctuation) $ splitOn isPunctuation numbersRow
  let boardSize = 5
  let boards = createBoards (drop 1 rows) boardSize
  print $ ex1 boards numbers
  print $ ex2 boards numbers
  return ()

toInt :: String -> Int
toInt = read

type Mark = Bool

type Cell = (Int, Mark)

type Row = [Cell]

type Board = [Row]

-- Takes a predicate and a list. Splits given list as soon as the predicate
-- matches. For each split a a new list will be added to our list of lists.
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred = groupBy (\_ next -> (not . pred) next)

-- removes space from left and right of given string
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- removes items from left and right of given list based on given predicate.
trimWith :: (a -> Bool) -> [a] -> [a]
trimWith pred = dropWhileEnd pred . dropWhile pred

-- goes through given bord and marks Cell with given if the value matches the
-- given int.
markBoard :: Int -> Board -> Board
markBoard number =
  map
    (\row ->
       map
         (\(val, mark) ->
            if val == number
              then (val, True)
              else (val, mark))
         row)

makeCell :: Int -> Cell
makeCell val = (val, False)

isMarked :: Cell -> Bool
isMarked (_, marked) = marked

makeRow :: [Char] -> Row
makeRow =
  map (makeCell . toInt . trim) . filter (not . all isSpace) . splitOn isSpace
  where
    toInt x = read x :: Int

unmarkedNumbers' :: Board -> Board
unmarkedNumbers' board = map (\row -> filter (not . isMarked) row) board

-- NOTE: Hlint likes this one more than the one above. I think in the one above
-- it is more clear what actually happens. TODO: ask for opinion.
unmarkedNumbers :: Board -> Board
unmarkedNumbers = map (filter (not . isMarked))

sumOfBoard' :: Board -> Int
sumOfBoard' board = sum $ map (\row -> sum $ map (\(val, _) -> val) row) board

-- NOTE: Hlint likes this one more than the one above. I think in the one above
-- it is more clear what actually happens. TODO: ask for opinion.
sumOfBoard :: Board -> Int
sumOfBoard board = sum $ map (sum . map fst) board

--
-- finds winning board and calculates the result
ex1 :: [Board] -> [Int] -> Int
-- no board found. error out.
ex1 boards [] = undefined
ex1 boards (x:numbers) =
  let markedBoards = map (markBoard x) boards
      selectedBoards = filter isWinningBoard markedBoards
   in case length selectedBoards of
        1 -> x * sumOfBoard (unmarkedNumbers $ head selectedBoards)
        _ -> ex1 markedBoards numbers

-- finds losing board, then plays until it wins.
ex2 :: [Board] -> [Int] -> Int
-- no board found. error out.
ex2 boards [] = undefined
ex2 boards (x:numbers) =
  let markedBoards = map (markBoard x) boards
      selectedBoards = filter (not . isWinningBoard) markedBoards
   in case length selectedBoards of
        1 ->
          let losingBoard = head selectedBoards
           in ex1 [losingBoard] numbers
        _ -> ex2 markedBoards numbers

createBoards :: [String] -> Int -> [Board]
createBoards rows size
  | length rows < size = []
  | otherwise =
    let (boardRows, rest) = splitAt size rows
        board = map makeRow boardRows
     in board : createBoards rest size

isWinningBoard :: Board -> Bool
isWinningBoard board =
  let rows = board
      cols = transpose board
   in any isWinningRow $ rows ++ cols

isWinningRow :: Row -> Bool
isWinningRow = all isMarked
