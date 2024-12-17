{-# LANGUAGE TupleSections #-}

module Day11 (run, part1, part2, blink, blinkTimes, blinkTimesWithMemoization) where

import Common (Part (P1, P2), Runner, splitStringByFn)
import Data.Map qualified
import Debug.Trace (traceShow)
import GHC.Float (float2Int, int2Float)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = length $ blinkTimes (handleInput input) 25

part2 :: String -> Int
part2 input = length $ blinkTimesWithMemoization (handleInput input) 75

handleInput :: String -> [Int]
handleInput input = map (\x -> read x :: Int) $ splitStringByFn input (== ' ')

numDigits :: Int -> Int
numDigits x
  | x == 0 = 0
  | otherwise = 1 + numDigits (x `div` 10)

partitionInt :: Int -> [Int]
partitionInt x = do
  let y = float2Int (10 ** int2Float (numDigits x `div` 2))
  [x `div` y, x `mod` y]

blink :: [Int] -> [Int]
blink (0 : rest) = 1 : blink rest
blink (h : rest)
  | even (numDigits h) = partitionInt h ++ blink rest
  | otherwise = h * 2024 : blink rest
blink [] = []

blinkTimes :: [Int] -> Int -> [Int]
blinkTimes stones 0 = stones
blinkTimes stones times = blinkTimes (blink stones) (times - 1)

-- (value, times to blink)
type MStone = (Int, Int)

blinkWithMemoization :: MStone -> Data.Map.Map MStone [MStone] -> (Data.Map.Map MStone [MStone], [MStone])
blinkWithMemoization (val, blinks) memoized = do
  case Data.Map.lookup (val, blinks) memoized of
    Nothing -> do
      let newStones = map (,blinks - 1) (blink [val])
      let newMemoized = Data.Map.insert (val, blinks) newStones memoized
      (newMemoized, newStones)
    Just memoVals -> (memoized, memoVals)

blinkTimesWithMemoization :: [Int] -> Int -> [MStone]
blinkTimesWithMemoization stones times = do
  blinkTimesWithMemoization' (map (,times) stones) Data.Map.empty

blinkTimesWithMemoization' :: [MStone] -> Data.Map.Map MStone [MStone] -> [MStone]
blinkTimesWithMemoization' ((val, 0) : rest) memoized = (val, 0) : blinkTimesWithMemoization' rest memoized
blinkTimesWithMemoization' (h : rest) memoized = do
  let (newMemoized, newMStones) = blinkWithMemoization h memoized
  let nextMemoized = Data.Map.map (\v -> if h `elem` v then filter (/= h) v ++ newMStones else v) newMemoized
  blinkTimesWithMemoization' (newMStones ++ rest) nextMemoized
blinkTimesWithMemoization' [] memoized = []
