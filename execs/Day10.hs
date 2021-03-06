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
-- using state monad to keep track of what we have parsed as our state.
-- src: http://learnyouahaskell.com/for-a-few-monads-more
import Control.Monad.State
import Data.Either (lefts, rights)

import Data.List (sort)

main
  -- input <- lines <$> readFile "testInput"
 = do
  input <- lines <$> readFile "inputDay10"
  print $ ex1 input
  print $ ex2 input
  return ()

ex1 :: [[Char]] -> Int
ex1 input =
  foldl (\score val -> score + getScore val calculateSyntaxScore) 0 $
  lefts $ map parseLine input

-- Using foldM to chain the result from the previous parse result to the next
-- parse call.
parseLine :: [Char] -> Either Char [Char]
parseLine = foldM parse []

-- Uses Either Monad to parse our input. Whenever we have a closing tag that
-- does not match our latest opening tag, we have a syntax error. This error is
-- represented as a Left 'charHere'.
parse :: [Char] -> Char -> Either Char [Char]
parse parsed nextChar
  | isOpeningTag nextChar = Right (parsed ++ [nextChar])
  | isClosingTag nextChar =
    if last parsed == getMatchingTag nextChar
      then Right $ init parsed
      else Left nextChar

--- calculates score for the autocomplete tooling
ex2 :: [[Char]] -> Int
ex2 = getMiddle . sort . map (scoreStrings . complete) . rights . map parseLine
  where
    scoreStrings :: [Char] -> Int
    scoreStrings =
      foldl
        (\result char -> 5 * result + getScore char calculateAutocompleteScore)
        0

-- Function which takes a char and a function which maps a char to a value. Used
-- for calculating syntax and autocomplete Scores.
getScore :: Char -> (Char -> Int) -> Int
getScore x f = f x

-- calculates score for a character for syntax checker (part 1)
calculateSyntaxScore :: Char -> Int
calculateSyntaxScore x
  | x == ')' = 3
  | x == ']' = 57
  | x == '}' = 1197
  | x == '>' = 25137
  | otherwise = error $ "symbol does not exist. Got the symbol: " ++ [x]

-- calculates scores for autocomplete tools :D (part 2)
calculateAutocompleteScore :: Char -> Int
calculateAutocompleteScore x
  | x == ')' = 1
  | x == ']' = 2
  | x == '}' = 3
  | x == '>' = 4
  | otherwise = error $ "symbol does not exist. Got the symbol: " ++ [x]

getMiddle :: [Int] -> Int
getMiddle x
  | even $ length x = error "can't get middle for even amount of items!"
  | otherwise = x !! (length x `div` 2)

-- returns the list of chars which complete a given incomplete line. (e.g.
-- complete "[(<" == ">)]"
complete :: [Char] -> [Char]
complete = foldl (\result char -> getMatchingTag char : result) ""

-- Takes a closing tag and returns the corresponding opening tag (and vice
-- versa).
getMatchingTag :: Char -> Char
getMatchingTag tag
  | tag == ')' = '('
  | tag == ']' = '['
  | tag == '}' = '{'
  | tag == '>' = '<'
  | tag == '(' = ')'
  | tag == '[' = ']'
  | tag == '{' = '}'
  | tag == '<' = '>'
  | otherwise = error $ "no matching tag was found! Tag given: " ++ [tag]

isClosingTag :: Char -> Bool
isClosingTag char = char `elem` [')', ']', '}', '>']

isOpeningTag :: Char -> Bool
isOpeningTag char = char `elem` ['(', '[', '{', '<']
