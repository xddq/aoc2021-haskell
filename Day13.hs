{-|
Example data:
-}
import Data.List (foldl')
import Data.List.Split (splitOn)

import Data.Map (Map)
import qualified Data.Map as M

main
  -- input <- lines <$> readFile "inputDay11"
 = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  -- print $ ex2 input
  return ()

type Pos = Int

data FoldMode
  = Vertical
  | Horizontal
  deriving (Show, Read, Eq)

type Dots = Map Int (Map Int Int)

-- ex1 :: [[Char]] -> Int
ex1 input =
  let (dots, _:instructionsString) = break (== "") input
      dotMap
      -- TODO(pierre): Why do I need to wrap mkDot $ splitOn ... into []?
       = mkDotMap $ [(x, y) | dot <- dots, (x, y) <- [mkDot $ splitOn "," dot]]
      instructions = map parseInstruction instructionsString
   in foldl' (foldPaper) dotMap instructions
  where
    parseInstruction :: String -> (FoldMode, Pos)
    parseInstruction instruction =
      let axis:_:amount = last $ words instruction
       in (parseMode axis, read amount)
    parseMode :: Char -> FoldMode
    parseMode input =
      case input of
        'x' -> Horizontal
        'y' -> Vertical
        otherwise ->
          error $
          "This case should not happen. parseMode function got input: " ++
          [input]

foldPaper :: Dots -> (FoldMode, Pos) -> Dots
foldPaper dots (mode, pos)
  | mode == Horizontal = dots
  | mode == Vertical = dots

-- expects given list of strings to be something like ["1","12"] and transforms
-- it into (1,12)
mkDot :: [String] -> (Int, Int)
mkDot (x:y:_) = (read x, read y)

mkDotMap :: [(Int, Int)] -> Map Int (Map Int Int)
mkDotMap = foldl' (\acc (x, y) -> M.insert x (M.insert y 1 M.empty) acc) M.empty
