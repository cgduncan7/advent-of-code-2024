module Day9 (run, part1, part2) where

import Common (Part (P1, P2), Runner, head', last', rest)
import Data.List (find, partition)
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = checksum $ compactDisk $ handleLine input True 0 0

part2 :: String -> Int
part2 _ = 2

data Block = Empty Int | File Int deriving (Eq, Show)

-- Bool represents mode; True = File and False = Empty
-- Int is the file id and the other is empty id
handleLine :: String -> Bool -> Int -> Int -> [Block]
handleLine (h : rest) True id eid = replicate (read [h] :: Int) (File id) ++ handleLine rest False (id + 1) eid
handleLine (h : rest) False id eid = replicate (read [h] :: Int) (Empty eid) ++ handleLine rest True id (eid + 1)
handleLine [] _ _ _ = []

isEmpty :: Block -> Bool
isEmpty (Empty _) = True
isEmpty _ = False

compactDisk :: [Block] -> [Block]
compactDisk ((Empty _) : r) = do
  let (f : r') = dropWhile isEmpty (reverse r)
  f : compactDisk (reverse r')
compactDisk (h : r) = h : compactDisk r
compactDisk [] = []

checksum :: [Block] -> Int
checksum blocks = foldr (\(f, idx) acc -> case f of File i -> acc + i * idx; Empty _ -> acc) 0 (zip blocks [0, 1 ..])
