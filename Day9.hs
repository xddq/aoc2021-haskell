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
  -- input <- lines <$> readFile "inputDay9"
 = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  return () -- ex1 :: [Int] -> Int

-- prefix with P since Left and Right are already taken for Either!
data Position a
  = PLeft a
  | PAbove a
  | PRight a
  | PBelow a
  | PMiddle a
  | PEdge
  deriving (Show, Read, Eq)

instance (Ord a, Eq a) => Ord (Position a) where
  -- compare (PLeft x) (PLeft y) = compare x y
  -- TODO(pierre): how can I implement the ord contrain? Why do I get an error
  -- for patterm matching with _?
  compare (_ x) (_ y) = compare x y

-- MAYBE(pierre): Is it possibile to define that first positon must be
-- PLeft etc..?
type Point
   = (Position Int, Position Int, Position Int, Position Int, Position Int)

-- ex1 :: [[Char]] -> [[Maybe Int]]
ex1 input =
  let rows = parseRow (\pos -> PLeft pos) (\pos -> PRight pos) input
      cols =
        parseRow (\pos -> PAbove pos) (\pos -> PBelow pos) $ transpose input
      combinedResult =
        [ (left, above, middle, right, below)
        | ((left, middle, right):row) <- rows
        , ((above, _, below):col) <- (transpose cols)
        ]
   in filter isLowPoint combinedResult
  where
    isLowPoint :: Point -> Bool
    isLowPoint (left, above, middle, right, below) = middle < left
    parseRow ::
         (Int -> Position Int)
      -> (Int -> Position Int)
      -> [[Char]]
      -> [[(Position Int, Position Int, Position Int)]]
    parseRow left right input =
      let rows = map (\row -> map (\digit -> digitToInt digit) row) input
       in map
            (\row ->
               zip3
                 (PEdge : map left row)
                 (map PMiddle row)
                 ((drop 1 $ map right row) ++ [PEdge]))
            rows
