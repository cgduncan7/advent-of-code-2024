module Day2 (run, part1, part2) where

import Common (Part (P1, P2), Runner, head', splitStringByFn)
import Data.Foldable (foldl')
import Data.Maybe (isJust, isNothing)
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = length $ filter isReportSafe $ map (\l -> map (\word -> read word :: Int) $ splitStringByFn l (== ' ')) (lines input)

toDeltas :: [Int] -> [Int]
toDeltas ints = tail $ fst $ foldl' (\(d, p) c -> (d ++ [c - p], c)) ([], 0) ints

areDeltasSafe :: [Int] -> Bool
areDeltasSafe deltas = all (\x -> 1 <= x && x <= 3) deltas || all (\x -> -3 <= x && x <= -1) deltas

isReportSafe :: [Int] -> Bool
isReportSafe ints = areDeltasSafe (toDeltas ints)

part2 :: String -> Int
part2 input = length $ filter isReportSafeWithDampener $ map (\l -> map (\word -> read word :: Int) $ splitStringByFn l (== ' ')) (lines input)

isReportSafeWithDampener :: [Int] -> Bool
isReportSafeWithDampener i = do
  let deltas = toDeltas i
  areDeltasSafe deltas
    || areDeltasSafe (tail deltas)
    || areDeltasSafe (tail $ reverse deltas)
    || ( case countDeltas deltas of
           (_, 1, 0) -> any areDeltasSafe $ filterDeltas deltas Pos
           (_, 0, 1) -> any areDeltasSafe $ filterDeltas deltas Pos
           (1, _, 0) -> any areDeltasSafe $ filterDeltas deltas Neg
           (0, _, 1) -> any areDeltasSafe $ filterDeltas deltas Neg
           (p, n, z) -> False
       )

countDeltas :: [Int] -> (Int, Int, Int)
countDeltas = foldl' (\(p, n, z) c -> if c > 0 then (p + 1, n, z) else if c < 0 then (p, n + 1, z) else (p, n, z + 1)) (0, 0, 0)

data Sign = Pos | Neg

filterDeltas :: [Int] -> Sign -> [[Int]]
filterDeltas ints Pos = do
  let (filtered, (removedInt, removedIdx), _) = foldl' (\(filtered, removed, idx) cur -> if cur <= 0 then (filtered, (cur, idx), idx + 1) else (filtered ++ [cur], removed, idx + 1)) ([], (0, 0), 0) ints
  let indexedFiltered = zip filtered [0, 1 ..]
  [map (\(n, i) -> if i == removedIdx then n + removedInt else n) indexedFiltered, map (\(n, i) -> if i == removedIdx + 1 then n + removedInt else n) indexedFiltered]
filterDeltas ints Neg = do
  let (filtered, (removedInt, removedIdx), _) = foldl' (\(filtered, removed, idx) cur -> if cur >= 0 then (filtered, (cur, idx), idx + 1) else (filtered ++ [cur], removed, idx + 1)) ([], (0, 0), 0) ints
  let indexedFiltered = zip filtered [0, 1 ..]
  [map (\(n, i) -> if i == removedIdx then n + removedInt else n) indexedFiltered, map (\(n, i) -> if i == removedIdx + 1 then n + removedInt else n) indexedFiltered]
