{-|
Example data:
     0:      1:      2:      3:      4:
     aaaa    ....    aaaa    aaaa    ....
    b    c  .    c  .    c  .    c  b    c
    b    c  .    c  .    c  .    c  b    c
     ....    ....    dddd    dddd    dddd
    e    f  .    f  e    .  .    f  .    f
    e    f  .    f  e    .  .    f  .    f
     gggg    ....    gggg    gggg    ....

      5:      6:      7:      8:      9:
     aaaa    aaaa    aaaa    aaaa    aaaa
    b    .  b    .  .    c  b    c  b    c
    b    .  b    .  .    c  b    c  b    c
     dddd    dddd    ....    dddd    dddd
    .    f  e    f  .    f  e    f  .    f
    .    f  e    f  .    f  e    f  .    f
     gggg    gggg    ....    gggg    gggg
These are the possible segments/numbers we can display.
The wires/signals are mixed up. When we see an "a" it could signal any other
letter, same for "b","c", etc..
-}
-- for using Map. src for notation:
-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
import Data.Map (Map)
import qualified Data.Map as M

import Data.List

-- for splitOn
import Data.List.Split

{-|
Initial Plan:
    1) Use map for key/value retrieval. Store each digit (0..9) as key and their string representation as value.
    2) Decode the strings until we have each value decoded. First [1,4,7,8] which can be decoded by length. Then decode [2,3,5] with the help of values [1,4,7,8], then decode [0,6,9] with the help of values [1,4,7,8]. Try to do this in parallel (if I figure out how to do this somewhat quickly)
    3) For each input, create a map containing our decodings.
    4) Then use decodingMap and a digitCountMap consisting of key(digit) and value(sum of occurences) and the stuff we have to decode.
    5) for ex1 use sum of values for the keys 1,4,7,8 from digitCountMap
    6) for ex2 (don't know task yet) but probably take sum of the value of all our keys.
-}
main
  -- input <- readFile "inputDay8"
 = do
  (segments, outputValues) <- parseFile
  print $ ex1 segments outputValues
  print $ decodeSegments segments M.empty
  -- print $ ex2
  return ()

parseFile :: IO ([String], [String])
parseFile = do
  input <- readFile "inputDay8"
  let unparsedSegments:outputValues = splitOn "|" input
  return (words unparsedSegments, words $ head outputValues)

type Segment = String

type Count = Int

type Digit = Int

type Decoding = String

ex1 :: [Segment] -> [Segment] -> Count
ex1 segments outputVals =
  let decodings = decodeSegments segments M.empty
   in 1

decodeSegments :: [Segment] -> Map Digit Decoding -> Map Digit Decoding
decodeSegments segments decodings =
  let uniqueDecodings = inferUniqueSegments segments decodings
  -- MAYBE(pierre): figure out how to do in parallel?
      length5Segments = filter (\segment -> length segment == 5) segments
      newDecodings = inferLength5Segments length5Segments uniqueDecodings
      -- length6Segments = filter (\segment -> length segment == 6) segments
      -- finalDecodings = inferLength6Segments length6Segments newDecodings
   in newDecodings

inferUniqueSegments :: [Segment] -> Map Digit Decoding -> Map Digit Decoding
inferUniqueSegments [] decodings = decodings
inferUniqueSegments (segment:segments) decodings
  -- unique for digit 1
  | length segment == 2 =
    inferUniqueSegments segments (M.insert 1 segment decodings)
  -- unique for digit 4
  | length segment == 4 =
    inferUniqueSegments segments (M.insert 4 segment decodings)
  -- unique for digit 7
  | length segment == 3 =
    inferUniqueSegments segments (M.insert 7 segment decodings)
  -- unique for digit 8
  | length segment == 7 =
    inferUniqueSegments segments (M.insert 8 segment decodings)
  | otherwise = inferUniqueSegments segments decodings

-- Infer length 5 segments([2,3,5]) based on the amount of elements that are left after
-- intersection. Use only uniquely identifiable segmentns([1,4,7,8].
-- E.g. 3 \\ 1 = 2 elements; 2 \\ 1 = 1 elements; 5 \\ 1 = 1 elements;
-- From this we can infer that if the result is 2 elements, the segment is the
-- 3.
inferLength5Segments :: [Segment] -> Map Digit Decoding -> Map Digit Decoding
inferLength5Segments [] decodings = decodings
inferLength5Segments (segment:segments) decodings
  -- infers 2
  | length (segment \\ (decodings M.! 4)) == 2 =
    inferLength5Segments segments $ M.insert 2 segment decodings
  -- infers 3
  | length (segment \\ (decodings M.! 1)) == 2 =
    inferLength5Segments segments $ M.insert 3 segment decodings
  -- infers 5
  | otherwise = inferLength5Segments segments $ M.insert 5 segment decodings

-- Infer length 6 segments ([0,6,9]) based on the amount of elements that are left
-- after intersection. Similar to inferLength5Segments.
inferLength6Segments :: [Segment] -> Map Digit Decoding -> Map Digit Decoding
inferLength6Segments (segment:segments) decodings
  -- infers 6
  | length (segment \\ (decodings M.! 7)) == 2 = M.insert 6 segment decodings
  -- infers 9
  | length (segment \\ (decodings M.! 4)) == 2 = M.insert 6 segment decodings
  -- infers 0
  | otherwise = M.insert 0 segment decodings
