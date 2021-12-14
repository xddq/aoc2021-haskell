{-|
Example data:
    [({(<(())[]>[[{[]{<()<>>
    [(()[<>])]({[<{<<[]>>(
    {([(<{}[<>[]}>{[]{[(<()>
    (((({<>}<{<{<>}{[]{[]{}
    [[<[([]))<([[{}[[()]]]
    [{[{({}]{}}([{[{{{}}([]
    {<[[]]>}<{[{[{[]{()[[[]
    [<(<(<(<{}))><([]([]()
    <{([([[(<>()){}]>(<<{{
    <{([{{}}[<[[[<>{}]]]>[]]

    Syntax errors yield points:
    ): 3 points.
    ]: 57 points.
    }: 1197 points.
    >: 25137 points.
-}
import Data.Map (Map)
import qualified Data.Map as M

-- using state monad to keep track of what we have parsed as our state.
-- src: http://learnyouahaskell.com/for-a-few-monads-more
import Control.Monad.State
import Data.Either (fromLeft, isLeft, isRight)

-- imports only digitToInt from Data.Char
-- https://wiki.haskell.org/Import
import Data.Char (digitToInt, toUpper)
import Data.List (foldl', nub, sortBy, transpose)

main
  -- input <- lines <$> readFile "testInput"
 = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  -- print $ ex2 input
  return ()

getScore :: Char -> Int
getScore x
  | x == ')' = 3
  | x == ']' = 57
  | x == '}' = 1197
  | x == '>' = 25137
  | otherwise = error $ "symbol does not exist. Got the symbol: " ++ [x]

ex2 input = filter isRight . map parseLine

ex1 :: [[Char]] -> Int
ex1 =
  foldl
    (\score val ->
       case val of
         Right _ -> score
         Left x -> score + getScore x)
    0 .
  map parseLine

parseLine :: [Char] -> Either Char [Char]
parseLine = foldM parse []

-- takes a closing tag and returns the corresponding opening tag.
-- MAYBE(pierre): add cases for vice versa, but I think they are not needed.
getMatchingTag :: Char -> Char
getMatchingTag tag
  | tag == ')' = '('
  | tag == ']' = '['
  | tag == '}' = '{'
  | tag == '>' = '<'
  | otherwise = error $ "no matching tag was found! Tag given: " ++ [tag]

isClosingTag :: Char -> Bool
isClosingTag char = any (== char) [')', ']', '}', '>']

isOpeningTag :: Char -> Bool
isOpeningTag char = any (== char) ['(', '[', '{', '<']

parse :: [Char] -> Char -> Either Char [Char]
parse parsed nextChar
  | isOpeningTag nextChar = Right (parsed ++ [nextChar])
  | isClosingTag nextChar =
    if last parsed == getMatchingTag nextChar
      then Right $ init parsed
      else Left nextChar
