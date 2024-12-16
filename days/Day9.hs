{-# LANGUAGE LambdaCase #-}

module Day9 (run, part1, part2) where

import Common (Part (P1, P2), Runner, head', last', rest)
import Data.List (find, partition)
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = checksum $ compactDisk $ handleLine input True 0

part2 :: String -> Int
part2 input = do
  let blocks = handleLine input True 0
  let maxBlockId = foldr (\cur acc -> case cur of Empty _ -> acc; File (fsize, fid) -> max fid acc) 0 blocks
  checksum $ compactDisk' blocks maxBlockId

data Block = Empty Int | File (Int, Int) deriving (Eq, Show)

-- Bool represents mode; True = File and False = Empty
-- Int is the file id and the other is empty id
handleLine :: String -> Bool -> Int -> [Block]
handleLine ('0' : rest) flag id = handleLine rest (not flag) id
handleLine (h : rest) True id = File (read [h] :: Int, id) : handleLine rest False (id + 1)
handleLine (h : rest) False id = Empty (read [h] :: Int) : handleLine rest True id
handleLine [] _ _ = []

isEmpty :: Block -> Bool
isEmpty (Empty _) = True
isEmpty _ = False

compactDisk :: [Block] -> [Block]
compactDisk (Empty size : r) = do
  let (File (fsz, id) : r') = dropWhile isEmpty (reverse r)
  let reAdd = fsz - 1 > 0
  case size - 1 of
    0 -> File (1, id) : compactDisk (reverse r' ++ ([File (fsz - 1, id) | reAdd]))
    sz -> File (1, id) : compactDisk (Empty sz : reverse r' ++ ([File (fsz - 1, id) | reAdd]))
compactDisk (h : r) = h : compactDisk r
compactDisk [] = []

find' :: (a -> Bool) -> [a] -> [a]
find' fn list = case find fn list of Nothing -> []; Just x -> [x]

partitionInThree :: [a] -> (a -> Bool) -> ([a], [a], [a])
partitionInThree list fn = do
  let lastTwo = dropWhile (not . fn) list
  let middle = takeWhile fn lastTwo
  let first = takeWhile (not . fn) list
  let end = dropWhile fn lastTwo
  (first, middle, end)

compactDisk' :: [Block] -> Int -> [Block]
compactDisk' blocks (-1) = blocks
compactDisk' blocks fileId = do
  let (before, files, after) = partitionInThree blocks (\case File (fsize, fid) -> fid == fileId; _ -> False)
  let File (fsize, fid) = head files
  let (beforeBefore, beforeEmpties, beforeAfter) = partitionInThree before (\case (Empty esize) -> esize >= fsize; _ -> False)
  case beforeEmpties of
    [Empty esize] ->
      if esize > fsize
        then compactDisk' (beforeBefore ++ [File (fsize, fid), Empty (esize - fsize)] ++ beforeAfter ++ [Empty fsize] ++ after) (fileId - 1)
        else compactDisk' (beforeBefore ++ [File (fsize, fid)] ++ beforeAfter ++ [Empty fsize] ++ after) (fileId - 1)
    _ -> compactDisk' (before ++ files ++ after) (fileId - 1)

checksum :: [Block] -> Int
checksum blocks =
  fst $
    foldl
      ( \(accTotal, accIndex) f -> case f of
          File (size, id) ->
            ( accTotal + foldr (\(i, x) acc -> acc + (i * x)) 0 (zip (replicate size id) [accIndex, (accIndex + 1) ..]),
              accIndex + size
            )
          (Empty size) -> (accTotal, accIndex + size)
      )
      (0, 0)
      blocks
